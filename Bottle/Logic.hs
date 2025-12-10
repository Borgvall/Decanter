{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic where

import Bottle.Types
import System.Process.Typed
import System.Directory 
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , removePathForcibly
    , findExecutable
    , doesFileExist
    )
import System.FilePath ((</>), takeExtension)
import Control.Exception (try, IOException)
import Control.Monad (void, filterM)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs
import System.Environment (getEnvironment)

import Logic.Translation (tr)

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> [(String, String)]
getWineOverrides Bottle{..} =
    [ ("WINEPREFIX", bottlePath)
    , ("WINEARCH", archToString arch)
    ]

-- | Erstellt die Umgebungsvariablen für Wine, indem die aktuelle Umgebung
-- | gelesen und die Overrides eingefügt werden.
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv bottle = do
    let wineSpecificEnv = getWineOverrides bottle
    -- Automatische Ableitung der zu filternden Keys (deine Optimierung)
    let overrideKeys = map fst wineSpecificEnv 
    
    currentEnv <- getEnvironment
    
    -- Filtern der aktuellen Umgebung, um die Schlüssel zu entfernen,
    let filteredEnv = filter (\(k, _) -> k `notElem` overrideKeys) currentEnv
    
    -- Neue Umgebung erstellen: Overrides zuerst, dann der Rest der Umgebung.
    return (wineSpecificEnv ++ filteredEnv)

isWinetricksAvailable :: IO Bool
isWinetricksAvailable = do
    path <- findExecutable "winetricks"
    return (isJust path)

runWinetricks :: Bottle -> IO ()
runWinetricks bottle = runCmd bottle "winetricks" []

-- | Helper um Prozesse zu starten
runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = do
  mergedEnv <- getMergedWineEnv bottle
  void $ startProcess $ setEnv mergedEnv $ proc cmd args

-- | Bestimmt das Basisverzeichnis für alle Bottles: ~/.local/share/haskell-bottles
getBottlesBaseDir :: IO FilePath
getBottlesBaseDir = do
  base <- getXdgDirectory XdgData "haskell-bottles"
  createDirectoryIfMissing True base
  return base

-- | Scannt das Verzeichnis nach existierenden Bottles
listExistingBottles :: IO [Bottle]
listExistingBottles = do
  base <- getBottlesBaseDir
  exists <- doesDirectoryExist base
  if not exists 
    then return []
    else do
      entries <- listDirectory base
      dirs <- filterM (\e -> doesDirectoryExist (base </> e)) entries
      validDirs <- filterM (\e -> doesDirectoryExist (base </> e </> "drive_c")) dirs
      return $ map (\name -> Bottle (T.pack name) (base </> name) SystemWine Win64) validDirs

createVolume :: FilePath -> IO ()
createVolume path = do
  result <- try (Btrfs.createSubvol path) :: IO (Either IOException ())
  case result of
    Right _ -> putStrLn $ "BTRFS Subvolume erstellt: " ++ path
    Left _  -> do
      putStrLn "Kein BTRFS oder Fehler, nutze Standard-Verzeichnis."
      createDirectoryIfMissing True path

data NameValid
  = Valid
  | EmptyName
  | NameTooLong
  | ContainsSlash
  deriving (Show, Eq)

-- | Überprüft, ob ein Bottle-Name gültig ist, und gibt den Grund für die Ungültigkeit zurück.
checkNameValidity :: T.Text -> NameValid
checkNameValidity name
  | T.null name = EmptyName
  | T.length name > 256 = NameTooLong
  | T.elem '/' name = ContainsSlash
  | otherwise = Valid

-- | Erklärt den Grund der Ungültigkeit in übersetztem Text.
explainNameValid :: NameValid -> T.Text
explainNameValid status = case status of
  Valid         -> ""
  EmptyName     -> tr "The name cannot be empty."
  NameTooLong   -> tr "The name is too long (max 256 characters)."
  ContainsSlash -> tr "The name cannot contain a slash ('/')."

-- | Erstellt ein Bottle-Objekt mit korrektem Pfad basierend auf Namen
createBottleObject :: T.Text -> Arch -> IO Bottle
createBottleObject name arch = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack name
  return $ Bottle name path SystemWine arch

-- Hauptlogik zum Erstellen (FIX: IO-Bindung für mergedEnv)
createBottleLogic :: Bottle -> IO ()
createBottleLogic bottle@Bottle{..} = do
  -- Just in case the GUI prevention is screwed
  case checkNameValidity bottleName of
    Valid -> do
      createVolume bottlePath
      
      mergedEnv <- getMergedWineEnv bottle
      let procConfig = setEnv mergedEnv $ proc "wineboot" ["-u"]
      
      runProcess_ procConfig
    invalidName -> do
      putStrLn $ "Ignoring creation with invalid bottle name '" ++ T.unpack bottleName ++ "': " ++ T.unpack (explainNameValid invalidName)

-- NEU: Logik zum Löschen des Wine-Prefix
deleteBottleLogic :: Bottle -> IO ()
deleteBottleLogic Bottle{..} = do
  putStrLn $ "Lösche Wine-Prefix: " ++ bottlePath
  removePathForcibly bottlePath
  putStrLn "Löschvorgang abgeschlossen."


-- Tools
runWineCfg :: Bottle -> IO ()
runWineCfg bottle = runCmd bottle "wine" ["winecfg"]

runRegedit :: Bottle -> IO ()
runRegedit bottle = runCmd bottle "wine" ["regedit"]

runUninstaller :: Bottle -> IO ()
runUninstaller bottle = runCmd bottle "wine" ["uninstaller"]

runFileManager :: Bottle -> IO ()
runFileManager Bottle{..} = do
  let driveC = bottlePath </> "drive_c"
  void $ startProcess $ proc "xdg-open" [driveC]

runExecutable :: Bottle -> FilePath -> IO ()
runExecutable bottle filePath = do
  let ext = takeExtension filePath
  if ext == ".msi"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]
