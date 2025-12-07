{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic where

import Bottle.Types
import System.Process.Typed
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeExtension)
import Control.Exception (try, IOException)
import Control.Monad (void)
import qualified System.Linux.Btrfs as Btrfs -- Aus dem Paket 'linux-btrfs'

-- Umgebungsvariablen setzen
getWineEnv :: Bottle -> [(String, String)]
getWineEnv Bottle{..} =
  [ ("WINEPREFIX", bottlePath)
  , ("WINEARCH", archToString arch)
  ]

-- Helper um Prozesse zu starten
runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = 
  void $ startProcess $ setEnv (getWineEnv bottle) $ proc cmd args

-- Erstellt Volume: Versucht BTRFS Subvolume, Fallback auf mkdir
createVolume :: FilePath -> IO ()
createVolume path = do
  -- Wir versuchen ein Subvolume zu erstellen.
  -- Das schlägt fehl, wenn das Dateisystem kein BTRFS ist oder Rechte fehlen.
  result <- try (Btrfs.createSubvolume path) :: IO (Either IOException ())
  
  case result of
    Right _ -> putStrLn $ "BTRFS Subvolume erstellt: " ++ path
    Left _  -> do
      putStrLn "Kein BTRFS oder Fehler, nutze Standard-Verzeichnis."
      createDirectoryIfMissing True path

-- Hauptlogik zum Erstellen
createBottleLogic :: Bottle -> IO ()
createBottleLogic bottle@Bottle{..} = do
  createVolume bottlePath
  -- wineboot initialisiert das Prefix
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

-- Führt EXE oder MSI aus
runExecutable :: Bottle -> FilePath -> IO ()
runExecutable bottle filePath = do
  let ext = takeExtension filePath
  if ext == ".msi"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]
