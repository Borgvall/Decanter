{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic 
  ( -- * Bottle Management
    listExistingBottles
  , getAvailableRunners
  , createBottleObject
  , createBottleLogic
  , deleteBottleLogic
  , checkSystemWine32Support
  , getSupportedArchitectures
  
    -- * Validation
  , checkNameValidity
  , NameValid(Valid)
  , explainNameValid
  
    -- * Running Programs
  , runExecutable
  , runFileWithStart
  , runWindowsLnk
  , killBottleProcesses
  
    -- * System Tools
  , runWineCfg
  , runRegedit
  , runUninstaller
  , isWinetricksAvailable
  , runWinetricks
  , runFileManager
  , findWineStartMenuLnks
  
    -- * Snapshots & BTRFS
  , isSnapshotableBottle
  , listSnapshots
  , createSnapshotLogic
  , restoreSnapshotLogic
  , deleteSnapshotLogic
  , openSnapshotFileManager
  ) where

import Bottle.Types
import System.Process.Typed
import System.Directory 
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    , findExecutable
    , getHomeDirectory
    )
import System.FilePath ((</>), takeExtension)
import Control.Exception (try, IOException, SomeException)
import Control.Monad (void, filterM, forM, forM_)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs
import System.Environment (getEnvironment)
import System.IO.Error
import Data.Char (isDigit)
import System.Exit (ExitCode(..))

import Logic.Translation (tr)

-- | Pfad zur Konfigurationsdatei innerhalb einer Bottle
getConfigPath :: FilePath -> FilePath
getConfigPath bottleDir = bottleDir </> "decanter.cfg"

-- | Speichert die Bottle-Konfiguration (Runner, Arch)
saveBottleConfig :: Bottle -> IO ()
saveBottleConfig b = do
    let content = show (runner b, arch b)
    writeFile (getConfigPath (bottlePath b)) content

-- | Lädt die Bottle-Konfiguration
loadBottleConfig :: FilePath -> IO (Maybe (RunnerType, Arch))
loadBottleConfig bottleDir = do
    let path = getConfigPath bottleDir
    exists <- doesFileExist path
    if exists 
        then do
            content <- readFile path
            -- Einfaches 'reads' für sicheres Parsen
            case reads content of
                [((r, a), "")] -> return (Just (r, a))
                _              -> return Nothing
        else return Nothing

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> [(String, String)]
getWineOverrides Bottle{..} =
    [ ("WINEPREFIX", bottlePath)
    , ("WINEARCH", archToString arch)
    ]

-- | Erstellt die Umgebungsvariablen für Wine/Proton
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv bottle = do
    let wineSpecificEnv = getWineOverrides bottle
    let overrideKeys = map fst wineSpecificEnv 
    
    currentEnv <- getEnvironment
    
    let filteredEnv = filter (\(k, _) -> k `notElem` overrideKeys) currentEnv
    
    -- Für Proton/umu: GAMEID ist oft notwendig (setzen wir auf generisch "nonsteam")
    -- WINEPREFIX wird bereits durch wineSpecificEnv gesetzt.
    let extraEnv = case runner bottle of
                     SystemWine -> []
                     Proton p   -> [("GAMEID", "nonsteam"), ("PROTONPATH", p)]

    return (wineSpecificEnv ++ extraEnv ++ filteredEnv)

isWinetricksAvailable :: IO Bool
isWinetricksAvailable = do
    path <- findExecutable "winetricks"
    return (isJust path)

runWinetricks :: Bottle -> IO ()
runWinetricks bottle = runCmd bottle "winetricks" []

-- | Helper um Prozesse zu starten (asynchron)
-- Passt den Befehl an, falls Proton verwendet wird.
runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = do
  mergedEnv <- getMergedWineEnv bottle
  
  case runner bottle of
    SystemWine -> 
        void $ startProcess $ setEnv mergedEnv $ proc cmd args
        
    Proton _ -> do -- Proton
        -- Wenn der Befehl "wine" ist, ersetzen wir ihn durch "umu-run".
        -- Andere Tools (wie winetricks) müssen eventuell gesondert behandelt werden,
        -- aber umu-run kann oft auch einfach davor gesetzt werden.
        let (realCmd, realArgs) = if cmd == "wine"
                                  then ("umu-run", args)
                                  else (cmd, args) -- Vorerst unverändert für andere Tools
        
        -- TODO: Prüfen, ob umu-run im PATH ist oder explizit konfiguriert werden muss
        void $ startProcess $ setEnv mergedEnv $ proc realCmd realArgs

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
    result <- try (Btrfs.getSubvolReadOnly path) :: IO (Either IOException Bool)
    case result of
        Right _ -> return True  -- Aufruf erfolgreich -> Es ist ein Subvolume
        Left _  -> return False -- Fehler -> Kein Subvolume (oder FS Error)

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
          
          -- Config laden
          maybeConfig <- loadBottleConfig path
          
          case maybeConfig of
            Just (r, a) -> return $ Bottle (T.pack name) path r a
            Nothing -> do
                -- Fallback für alte Bottles ohne Config
                detectedArch <- detectBottleArch path
                return $ Bottle (T.pack name) path SystemWine detectedArch

getAvailableRunners :: IO [RunnerType]
getAvailableRunners = do
  sysWine <- findExecutable "wine"
  let wineList = if isJust sysWine then [SystemWine] else []

  home <- getHomeDirectory
  let compatDir = home </> ".local/share/Steam/compatibilitytools.d"
  
  protonList <- do
    exists <- doesDirectoryExist compatDir
    if exists
      then do
        entries <- listDirectory compatDir
        -- Einfache Prüfung: Ist es ein Verzeichnis?
        paths <- filterM (\e -> doesDirectoryExist (compatDir </> e)) entries
        return [ Proton (compatDir </> p) | p <- paths ]
      else return []

  return (wineList ++ protonList)

createVolume :: FilePath -> IO ()
createVolume path = do
  result <- try (Btrfs.createSubvol path) :: IO (Either IOException ())
  case result of
    Right _ -> putStrLn $ "BTRFS subvolume created: " ++ path
    Left _  -> do
      putStrLn "No BTRFS or error, using standard directory."
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

createBottleObject :: T.Text -> Arch -> RunnerType -> IO Bottle
createBottleObject name arch rType = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack name
  return $ Bottle name path rType arch

createBottleLogic :: Bottle -> IO ()
createBottleLogic bottle@Bottle{..} = do
  case checkNameValidity bottleName of
    Valid -> do
      createVolume bottlePath
      
      -- NEU: Konfiguration speichern
      saveBottleConfig bottle
      
      mergedEnv <- getMergedWineEnv bottle
      
      -- Wir entfernen DISPLAY und WAYLAND_DISPLAY aus dem Environment, damit wineboot
      -- keine Fenster (wie den Gecko/Mono-Installer Dialog) öffnet.
      let headlessEnv = filter (\(k, _) -> k `notElem` ["DISPLAY", "WAYLAND_DISPLAY"]) mergedEnv
      
      -- Bei Proton nutzen wir auch wineboot (via umu-run wineboot?), 
      -- aber umu-run initialisiert das Prefix oft selbst beim ersten Start.
      -- Für Konsistenz rufen wir wineboot auf, passen aber den Befehl an.
      let bootCmd = case runner of
            SystemWine -> "wineboot"
            Proton _   -> "umu-run"
            
      let bootArgs = case runner of
            SystemWine -> ["-u"]
            Proton _   -> ["wineboot", "-u"]
      
      let procConfig = setEnv headlessEnv $ proc bootCmd bootArgs
      runProcess_ procConfig
    invalidName -> do
      putStrLn $ "Ignoring creation with invalid bottle name '" ++ T.unpack bottleName ++ "': " ++ T.unpack (explainNameValid invalidName)

-- | Safely deletes a BTRFS subvolume.
--
-- The initial 'setSubvolReadOnly' acts as a guard: it throws an exception
-- if the path is not a subvolume, preventing accidental deletion of
-- standard directories. If 'destroySubvol' fails with "Permission Denied"
-- (typical for non-root users), we fall back to standard recursive
-- directory deletion.
deleteSubvolumeForcible :: FilePath -> IO ()
deleteSubvolumeForcible subvolPath = do
  putStrLn $ "Forcing deletion of subvolume: " ++ subvolPath
  -- Erst Read-Only entfernen, sonst darf man nicht löschen
  Btrfs.setSubvolReadOnly subvolPath False
  destroyResult <- tryIOError (Btrfs.destroySubvol subvolPath)
  case destroyResult of
    Right () -> pure ()
    Left exception
      -- In case BTRFS is not mounted with user_subvol_rm_allowed,
      -- destroySubvol fails with "Permission Denied". The only work-around
      -- as a normal user is to delete the subvolume recursively as a
      -- directory.
      | isPermissionError exception -> removePathForcibly subvolPath
      -- Something unexpected happened, rethrow this error
      | otherwise -> ioError exception

-- | Löscht eine Bottle und alle zugehörigen Snapshots
deleteBottleLogic :: Bottle -> IO ()
deleteBottleLogic bottle@Bottle{..} = do
  putStrLn $ "Starting deletion process for: " ++ T.unpack bottleName
  
  -- WICHTIG: Laufende Prozesse beenden, bevor wir Dateien löschen.
  -- Dies verhindert Zombie-Wineserver, die spätere Tests oder Neuerstellungen blockieren.
  putStrLn "Stopping running processes..."
  _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

  -- 1. Snapshots löschen
  snaps <- listSnapshots bottle
  forM_ snaps $ \s -> do
      let path = snapshotPath s
      deleteSubvolumeForcible path

  -- 2. Den leeren Snapshot-Ordner der Bottle entfernen
  baseSnapDir <- getSnapshotsDir
  let bottleSnapDir = baseSnapDir </> T.unpack bottleName
  removePathForcibly bottleSnapDir

  -- 3. Die Bottle selbst löschen
  putStrLn $ "Deleting Wine prefix: " ++ bottlePath
  isSubvol <- isBtrfsSubvolume bottlePath
  if isSubvol
  then deleteSubvolumeForcible bottlePath
  else removePathForcibly bottlePath
  putStrLn "Deletion completed."

-- Tools
runWineCfg :: Bottle -> IO ()
runWineCfg bottle = runCmd bottle "wine" ["winecfg"]

runRegedit :: Bottle -> IO ()
runRegedit bottle = runCmd bottle "wine" ["regedit"]

runUninstaller :: Bottle -> IO ()
runUninstaller bottle = runCmd bottle "wine" ["uninstaller"]

-- | Führt xdg-open aus, aber bereinigt vorher das Environment von Nix-spezifischen
-- Variablen wie GI_TYPELIB_PATH. Dies verhindert, dass System-Anwendungen (wie Nautilus)
-- abstürzen, weil sie versuchen, inkompatible Bibliotheken aus dem Nix Store zu laden.
runSystemTool :: String -> [String] -> IO ()
runSystemTool tool args = do
  currentEnv <- getEnvironment
  -- Wir filtern GI_TYPELIB_PATH heraus. Dies ist der Hauptverursacher für
  -- "Namespace ... not available" Fehler in Python/GObject-Apps (Nautilus).
  let cleanEnv = filter (\(k, _) -> k /= "GI_TYPELIB_PATH") currentEnv
  void $ startProcess $ setEnv cleanEnv $ proc tool args

runFileManager :: Bottle -> IO ()
runFileManager Bottle{..} = do
  let driveC = bottlePath </> "drive_c"
  runSystemTool "xdg-open" [driveC]

runExecutable :: Bottle -> FilePath -> IO ()
runExecutable bottle filePath = do
  let ext = takeExtension filePath
  if ext == ".msi" || ext == ".MSI"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]

runFileWithStart :: Bottle -> FilePath -> IO ()
runFileWithStart bottle path = runCmd bottle "wine" ["start", "/unix", path]

-- | Beendet alle Prozesse in der Bottle (wineserver -k).
-- Dies sollte synchron geschehen, damit nachfolgende Operationen (wie Löschen) sicher sind.
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = do
  mergedEnv <- getMergedWineEnv bottle
  
  let (cmd, args) = case runner bottle of
        SystemWine -> ("wineserver", ["-k"])
        Proton _   -> ("umu-run", ["wineboot", "-k"])

  -- Wir nutzen runProcess_ statt startProcess, um zu warten bis der Befehl fertig ist.
  runProcess_ $ setEnv mergedEnv $ proc cmd args

runWindowsLnk :: Bottle -> FilePath -> IO ()
runWindowsLnk bottle lnkPath = runCmd bottle "wine" ["start", "/unix", lnkPath]

findWineStartMenuLnks :: Bottle -> IO [FilePath]
findWineStartMenuLnks Bottle{..} = do
    let driveC = bottlePath </> "drive_c"
    let commonStartMenu = driveC </> "ProgramData/Microsoft/Windows/Start Menu"
    let usersDir = driveC </> "users"
    usersExist <- doesDirectoryExist usersDir
    
    userStartMenus <- if usersExist
        then do
            users <- listDirectory usersDir
            return [ usersDir </> u </> "AppData/Roaming/Microsoft/Windows/Start Menu" | u <- users ]
        else return []

    let allSearchPaths = commonStartMenu : userStartMenus
    validPaths <- filterM doesDirectoryExist allSearchPaths
    concat <$> mapM findLnksRecursive validPaths

  where
    findLnksRecursive :: FilePath -> IO [FilePath]
    findLnksRecursive dir = do
        content <- listDirectory dir
        paths <- forM content $ \name -> do
            let path = dir </> name
            isDir <- doesDirectoryExist path
            if isDir
                then findLnksRecursive path 
                else if ".lnk" `isSuffixOf` name
                    then return [path]
                    else return []
        return (concat paths)

getSnapshotsDir :: IO FilePath
getSnapshotsDir = do
    base <- getXdgDirectory XdgData "Decanter"
    let snapDir = base </> "BottleSnapshots"
    createDirectoryIfMissing True snapDir
    return snapDir

isSnapshotableBottle :: Bottle -> IO Bool
isSnapshotableBottle = isBtrfsSubvolume . bottlePath

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
        in if null idPart
            then Nothing
            else case rest of
                ('_':name) -> 
                    let sId = read idPart :: Int
                        sName = T.pack name
                    in Just $ BottleSnapshot sId sName (parentDir </> filename)
                _ -> Nothing

getNextSnapshotId :: [BottleSnapshot] -> Int
getNextSnapshotId [] = 0
getNextSnapshotId snaps = maximum (map snapshotId snaps) + 1

createSnapshotLogic :: Bottle -> T.Text -> IO ()
createSnapshotLogic bottle sName = do
    baseSnapDir <- getSnapshotsDir
    let bottleSnapDir = baseSnapDir </> T.unpack (bottleName bottle)
    createDirectoryIfMissing True bottleSnapDir
    
    currentSnaps <- listSnapshots bottle
    let nextId = getNextSnapshotId currentSnaps
    
    let folderName = show nextId ++ "_" ++ T.unpack sName
    let destPath = bottleSnapDir </> folderName
    
    Btrfs.snapshot (bottlePath bottle) destPath True 

-- | Stellt eine Bottle aus einem Snapshot wieder her
restoreSnapshotLogic :: Bottle -> BottleSnapshot -> IO ()
restoreSnapshotLogic bottle snapshot = do
    putStrLn $ "Restoring bottle '" ++ T.unpack (bottleName bottle) ++ "' from snapshot " ++ show (snapshotId snapshot)
    
    -- Auch hier: Erst Prozess sicher beenden, bevor wir das Filesystem anfassen
    -- killBottleProcesses ist jetzt synchron und wartet auf Abschluss.
    _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())
    
    deleteSubvolumeForcible (bottlePath bottle)
    Btrfs.snapshot (snapshotPath snapshot) (bottlePath bottle) False
    putStrLn "Restore successful."

-- | Löscht einen spezifischen Snapshot
deleteSnapshotLogic :: BottleSnapshot -> IO ()
deleteSnapshotLogic snapshot = do
    putStrLn $ "Deleting snapshot: " ++ snapshotPath snapshot
    deleteSubvolumeForcible (snapshotPath snapshot)

-- | Öffnet den Dateimanager im drive_c des Snapshots
openSnapshotFileManager :: BottleSnapshot -> IO ()
openSnapshotFileManager snapshot = do
    let driveC = snapshotPath snapshot </> "drive_c"
    runSystemTool "xdg-open" [driveC]

-- | Prüft, ob das System 32-Bit Prefixe unterstützt.
-- Führt 'WINEARCH=win32 wine --version' aus. Wenn wine32 fehlt, gibt dies meist ExitCode 1 zurück.
checkSystemWine32Support :: IO Bool
checkSystemWine32Support = do
    currentEnv <- getEnvironment
    -- Wir überschreiben WINEARCH, behalten aber den Rest bei (z.B. PATH)
    let newEnv = ("WINEARCH", "win32") : filter ((/= "WINEARCH") . fst) currentEnv
    
    let procConfig = setEnv newEnv 
                   $ setStderr closed 
                   $ setStdout closed 
                   $ proc "wine" ["--version"]
                   
    result <- runProcess procConfig
    return (result == ExitSuccess)

-- | Gibt eine Liste der vom System unterstützten Architekturen zurück.
-- Win64 wird als immer verfügbar angenommen.
getSupportedArchitectures :: IO [Arch]
getSupportedArchitectures = do
    win32Support <- checkSystemWine32Support
    if win32Support 
       then return [Win64, Win32]
       else return [Win64]
