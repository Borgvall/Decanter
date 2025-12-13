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
    )
import System.FilePath ((</>), takeFileName)
import Control.Exception (try, IOException)
import Control.Monad (void, filterM, forM)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified System.Linux.Btrfs as Btrfs
import System.Environment (getEnvironment)
import Data.Char (isDigit)

import Logic.Translation (tr)

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> [(String, String)]
getWineOverrides Bottle{..} =
    [ ("WINEPREFIX", bottlePath)
    , ("WINEARCH", archToString arch)
    ]

-- | Erstellt die Umgebungsvariablen für Wine
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv bottle = do
    let wineSpecificEnv = getWineOverrides bottle
    let overrideKeys = map fst wineSpecificEnv 
    
    currentEnv <- getEnvironment
    
    let filteredEnv = filter (\(k, _) -> k `notElem` overrideKeys) currentEnv
    
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

-- | Bestimmt das Basisverzeichnis für alle Bottles
getBottlesBaseDir :: IO FilePath
getBottlesBaseDir = do
  base <- getXdgDirectory XdgData "Decanter"
  createDirectoryIfMissing True base
  return base

-- | Erkennt die Architektur anhand des Vorhandenseins von 'syswow64'
detectBottleArch :: FilePath -> IO Arch
detectBottleArch path = do
    let syswow64 = path </> "drive_c" </> "windows" </> "syswow64"
    is64 <- doesDirectoryExist syswow64
    return $ if is64 then Win64 else Win32


-- | Prüft, ob ein Pfad ein BTRFS Subvolume ist
isBtrfsSubvolume :: FilePath -> IO Bool
isBtrfsSubvolume path = do
    -- Wir versuchen Informationen über das Subvolumen abzurufen.
    -- Wenn das fehlschlägt (z.B. kein BTRFS oder kein Subvol), geben wir False zurück.
    result <- try (Btrfs.getSubvolInfo path) :: IO (Either IOException Btrfs.SubvolInfo)
    case result of
        Right _ -> return True
        Left _  -> return False

-- | Scannt das Verzeichnis nach existierenden Bottles und erkennt deren Architektur
listExistingBottles :: IO [Bottle]
listExistingBottles = do
  base <- getBottlesBaseDir
  exists <- doesDirectoryExist base
  if not exists 
    then return []
    else do
      entries <- listDirectory base
      
      -- Wir bauen den Pfad zusammen und prüfen, ob es ein Verzeichnis ist
      dirs <- filterM (\e -> doesDirectoryExist (base </> e)) entries
      
      -- Wir prüfen, ob 'drive_c' existiert (gültiges Prefix)
      validDirs <- filterM (\e -> doesDirectoryExist (base </> e </> "drive_c")) dirs
      
      -- Jetzt mappen wir über die validen Verzeichnisse und erkennen die Architektur
      forM validDirs $ \name -> do
          let path = base </> name
          detectedArch <- detectBottleArch path
          return $ Bottle (T.pack name) path SystemWine detectedArch

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

checkNameValidity :: T.Text -> NameValid
checkNameValidity name
  | T.null name = EmptyName
  | T.length name > 256 = NameTooLong
  | T.elem '/' name = ContainsSlash
  | otherwise = Valid

explainNameValid :: NameValid -> T.Text
explainNameValid status = case status of
  Valid         -> ""
  EmptyName     -> tr "The name cannot be empty."
  NameTooLong   -> tr "The name is too long (max 256 characters)."
  ContainsSlash -> tr "The name cannot contain a slash ('/')."

createBottleObject :: T.Text -> Arch -> IO Bottle
createBottleObject name arch = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack name
  return $ Bottle name path SystemWine arch

createBottleLogic :: Bottle -> IO ()
createBottleLogic bottle@Bottle{..} = do
  case checkNameValidity bottleName of
    Valid -> do
      createVolume bottlePath
      mergedEnv <- getMergedWineEnv bottle
      let procConfig = setEnv mergedEnv $ proc "wineboot" ["-u"]
      runProcess_ procConfig
    invalidName -> do
      putStrLn $ "Ignoring creation with invalid bottle name '" ++ T.unpack bottleName ++ "': " ++ T.unpack (explainNameValid invalidName)

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
  if ext == ".msi" || ext == ".MSI"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]

-- | Öffnet eine Datei (vom Host-System) innerhalb der Bottle
-- Nutzt 'start /unix', damit Wine Linux-Pfade korrekt auflöst.
runFileWithStart :: Bottle -> FilePath -> IO ()
runFileWithStart bottle path = runCmd bottle "wine" ["start", "/unix", path]

-- | Beendet alle Prozesse im Wineprefix via 'wineserver -k'
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = runCmd bottle "wineserver" ["-k"]

runWindowsLnk :: Bottle -> FilePath -> IO ()
runWindowsLnk bottle lnkPath = runCmd bottle "wine" ["start", "/unix", lnkPath]

-- | Scannt das Startmenü der Bottle nach .lnk Dateien
findWineStartMenuLnks :: Bottle -> IO [FilePath]
findWineStartMenuLnks Bottle{..} = do
    let driveC = bottlePath </> "drive_c"
    
    -- 1. Pfad für globale Startmenü-Einträge (ProgramData)
    let commonStartMenu = driveC </> "ProgramData/Microsoft/Windows/Start Menu"
    
    -- 2. Pfade für benutzerspezifische Einträge finden (users/*/AppData/...)
    let usersDir = driveC </> "users"
    usersExist <- doesDirectoryExist usersDir
    
    userStartMenus <- if usersExist
        then do
            users <- listDirectory usersDir
            return [ usersDir </> u </> "AppData/Roaming/Microsoft/Windows/Start Menu" | u <- users ]
        else return []

    -- Alle potenziellen Startmenü-Ordner zusammenfügen
    let allSearchPaths = commonStartMenu : userStartMenus

    -- Nur existierende Verzeichnisse durchsuchen
    validPaths <- filterM doesDirectoryExist allSearchPaths
    
    -- Rekursive Suche starten
    concat <$> mapM findLnksRecursive validPaths

  where
    -- Lokale Hilfsfunktion für rekursive Suche
    findLnksRecursive :: FilePath -> IO [FilePath]
    findLnksRecursive dir = do
        content <- listDirectory dir
        paths <- forM content $ \name -> do
            let path = dir </> name
            isDir <- doesDirectoryExist path
            if isDir
                then findLnksRecursive path -- Rekursiver Abstieg
                else if ".lnk" `isSuffixOf` name
                    then return [path]
                    else return []
        return (concat paths)

-- | Basisverzeichnis für Snapshots
getSnapshotsDir :: IO FilePath
getSnapshotsDir = do
    base <- getXdgDirectory XdgData "Decanter"
    let snapDir = base </> "BottleSnapshots"
    createDirectoryIfMissing True snapDir
    return snapDir

-- | Listet Snapshots für eine Bottle auf, sortiert nach ID
listSnapshots :: Bottle -> IO [BottleSnapshot]
listSnapshots bottle = do
    baseSnapDir <- getSnapshotsDir
    let bottleSnapDir = baseSnapDir </> T.unpack (bottleName bottle)
    
    exists <- doesDirectoryExist bottleSnapDir
    if not exists
        then return []
        else do
            entries <- listDirectory bottleSnapDir
            let snapshots = mapMaybe (parseSnapshotName bottleSnapDir) entries
            return $ sortOn snapshotId snapshots

  where
    parseSnapshotName :: FilePath -> String -> Maybe BottleSnapshot
    parseSnapshotName parentDir filename = 
        let (idPart, rest) = span isDigit filename
        in if null idPart || null rest || head rest /= '_'
            then Nothing
            else 
                let sId = read idPart :: Int
                    sName = T.pack $ drop 1 rest -- Den Unterstrich entfernen
                in Just $ BottleSnapshot sId sName (parentDir </> filename)

-- | Ermittelt die nächste freie ID
getNextSnapshotId :: [BottleSnapshot] -> Int
getNextSnapshotId [] = 0
getNextSnapshotId snaps = maximum (map snapshotId snaps) + 1

-- | Erstellt einen neuen Read-Only Snapshot
createSnapshotLogic :: Bottle -> T.Text -> IO ()
createSnapshotLogic bottle sName = do
    baseSnapDir <- getSnapshotsDir
    let bottleSnapDir = baseSnapDir </> T.unpack (bottleName bottle)
    createDirectoryIfMissing True bottleSnapDir
    
    currentSnaps <- listSnapshots bottle
    let nextId = getNextSnapshotId currentSnaps
    
    let folderName = show nextId ++ "_" ++ T.unpack sName
    let destPath = bottleSnapDir </> folderName
    
    -- Erstelle Read-Only Snapshot (True = readOnly)
    Btrfs.snapshot True (bottlePath bottle) destPath
