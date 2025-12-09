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
    , doesFileExist
    )
import System.FilePath ((</>), takeExtension)
import Control.Exception (try, IOException)
import Control.Monad (void, filterM)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs
import System.Environment (getEnvironment) -- WICHTIG: Sollte vorhanden sein


-- | Erstellt die Umgebungsvariablen für Wine, indem die aktuelle Umgebung
-- | (mit DISPLAY, XDG_RUNTIME_DIR, etc.) gelesen und WINEPREFIX/WINEARCH überschrieben wird.
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv Bottle{..} = do
  -- 1. Aktuelle Umgebung lesen (IO Aktion)
  currentEnv <- getEnvironment
  
  -- 2. Wine-spezifische Overrides definieren
  let wineSpecificEnv = 
        [ ("WINEPREFIX", bottlePath)
        , ("WINEARCH", archToString arch)
        ]
  
  -- 3. Aus der aktuellen Umgebung die Wine-spezifischen Keys entfernen,
  let filteredEnv = filter (\(k, _) -> k `notElem` ["WINEPREFIX", "WINEARCH"]) currentEnv
  
  -- 4. Neue Umgebung erstellen: Overrides zuerst, dann der Rest der Umgebung.
  return (wineSpecificEnv ++ filteredEnv)


-- | Helper um Prozesse zu starten (funktioniert bereits mit do-Notation)
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

-- | Erstellt ein Bottle-Objekt mit korrektem Pfad basierend auf Namen
createBottleObject :: T.Text -> Arch -> IO Bottle
createBottleObject name arch = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack name
  return $ Bottle name path SystemWine arch

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

-- Hauptlogik zum Erstellen
createBottleLogic :: Bottle -> IO ()
createBottleLogic bottle@Bottle{..} = do
  createVolume bottlePath
  
  -- FIX: Umgebung laden (IO-Aktion)
  mergedEnv <- getMergedWineEnv bottle
  
  -- Prozesskonfiguration mit der geladenen Umgebung erstellen
  let procConfig = setEnv mergedEnv $ proc "wineboot" ["-u"]
  
  runProcess_ procConfig

-- Tools
runWineCfg :: Bottle -> IO ()
runWineCfg bottle = runCmd bottle "winecfg" []

runRegedit :: Bottle -> IO ()
runRegedit bottle = runCmd bottle "regedit" []

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
