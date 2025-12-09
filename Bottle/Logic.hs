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

-- | Bestimmt das Basisverzeichnis für alle Bottles: ~/.local/share/bottles
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
-- Kriterium: Ordner existiert und enthält "drive_c"
listExistingBottles :: IO [Bottle]
listExistingBottles = do
  base <- getBottlesBaseDir
  exists <- doesDirectoryExist base
  if not exists 
    then return []
    else do
      entries <- listDirectory base
      -- Filter: Muss ein Verzeichnis sein
      dirs <- filterM (\e -> doesDirectoryExist (base </> e)) entries
      
      -- Filter: Muss drive_c enthalten (Indikator für valides Prefix)
      validDirs <- filterM (\e -> doesDirectoryExist (base </> e </> "drive_c")) dirs
      
      -- Rückgabe als Bottle Objekte
      -- Hinweis: Wir raten hier Arch/Runner, da wir die Config nicht parsen.
      -- In einer echten App würde man bottles.yml im Ordner lesen.
      return $ map (\name -> Bottle (T.pack name) (base </> name) SystemWine Win64) validDirs

-- Umgebungsvariablen setzen
getWineEnv :: Bottle -> [(String, String)]
getWineEnv Bottle{..} =
  [ ("WINEPREFIX", bottlePath)
  , ("WINEARCH", archToString arch)
  ]

runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = 
  void $ startProcess $ setEnv (getWineEnv bottle) $ proc cmd args

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
  let procConfig = setEnv (getWineEnv bottle)
                 $ proc "wineboot" ["-u"]
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
