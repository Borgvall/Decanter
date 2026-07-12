{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic
  ( -- * Bottle Management
    listExistingBottles
  , findBottleByName
  , findAppLnkByName
  , getAvailableRunners
  , getRunnerTypeDisplayName
  , createBottleObject
  , createBottleLogic
  , changeBottleRunnerLogic
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
  , runCmd

    -- * System Tools
  , runWineCfg
  , runRegedit
  , runUninstaller
  , isWinetricksAvailable
  , runWinetricks
  , runFileManager
  , findWineStartMenuLnks

    -- * Application Menu Integration
  , addToApplicationMenu
  , removeFromApplicationMenu
  , isInApplicationMenu

    -- * Direct3D Wrapper
  , Direct3DWrapperState(..)
  , Direct3DWrapperStatus(..)
  , WrapperHealth(..)
  , getDirect3DWrapperState
  , getDirect3DWrapperStatus
  , setDirect3DWrapperState
  , repairDirect3DWrapperState
  , isBottleReadyForWindowsApps
  ) where

import Bottle.Types
import Bottle.Logic.Process (getMergedWineEnv, killBottleProcesses, extractAppIcon)
import Bottle.Logic.Snapshots (isBtrfsSubvolume, deleteSubvolumeForcible, deleteAllSnapshots)
import Bottle.Logic.Direct3dWrappers
  ( Direct3DWrapperState(..)
  , Direct3DWrapperStatus(..)
  , WrapperHealth(..)
  , getDirect3DWrapperState
  , getDirect3DWrapperStatus
  , setDirect3DWrapperState
  , repairDirect3DWrapperState
  , isBottleReadyForWindowsApps
  )
import Logic.SystemTool (runSystemTool)
import System.Process.Typed
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , doesFileExist
    , doesPathExist
    , removePathForcibly
    , removeFile
    , createFileLink
    , findExecutable
    , getHomeDirectory
    )
import System.FilePath ((</>), takeExtension, takeBaseName)
import Control.Exception (try, IOException, SomeException)
import Control.Monad (void, filterM, forM)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy.Char8 as LBS8 -- for easy conversion of process output

import Logic.Translation (tr)
import Data.Vdf (extractDisplayName)

-- | Path to a bottle's configuration file
getConfigPath :: FilePath -> FilePath
getConfigPath bottleDir = bottleDir </> "decanter.cfg"

-- | Saves the bottle's configuration (runner, arch)
saveBottleConfig :: Bottle -> IO ()
saveBottleConfig b = do
    let content = show (runner b, arch b)
    writeFile (getConfigPath (bottlePath b)) content

-- | Loads the bottle's configuration
loadBottleConfig :: FilePath -> IO (Maybe (RunnerType, Arch))
loadBottleConfig bottleDir = do
    let path = getConfigPath bottleDir
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            -- Plain 'reads' for safe parsing
            case reads content of
                [((r, a), _)] -> return (Just (r, a))
                _              -> do
                    putStrLn $ "Could not parse: " ++ path
                    return Nothing
        else return Nothing

-- | Changes a bottle's runner (bottle object only, does not save)
changeBottleRunnerLogic :: Bottle -> RunnerType -> IO Bottle
changeBottleRunnerLogic bottle newRunner = do
  putStrLn $ "Changing runner for bottle '" ++ T.unpack (bottleName bottle)
             ++ "' from " ++ show (runner bottle)
             ++ " to " ++ show newRunner
  let updatedBottle = bottle { runner = newRunner }
  saveBottleConfig updatedBottle
  pure updatedBottle

isWinetricksAvailable :: IO Bool
isWinetricksAvailable = do
    path <- findExecutable "winetricks"
    return (isJust path)

runWinetricks :: Bottle -> IO ()
runWinetricks bottle = runCmd bottle "winetricks" []

-- | Helper to start processes (asynchronously).
-- Adjusts the command if Proton is used.
runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = do
  mergedEnv <- getMergedWineEnv bottle

  case runner bottle of
    SystemWine ->
        void $ startProcess $ setEnv mergedEnv $ proc cmd args

    Proton _ -> do
        -- If the command is "wine", replace it with "umu-run".
        -- Other tools (like winetricks) may need separate handling,
        -- but umu-run can often just be prepended to them too.
        let (realCmd, realArgs) = if cmd == "wine"
                                  then ("umu-run", args)
                                  else (cmd, args) -- left unchanged for other tools, for now

        -- TODO: check whether umu-run is on PATH or needs to be configured explicitly
        void $ startProcess $ setEnv mergedEnv $ proc realCmd realArgs

-- | Determines the base directory for all bottles
getBottlesBaseDir :: IO FilePath
getBottlesBaseDir = do
  base <- getXdgDirectory XdgData "Decanter"
  createDirectoryIfMissing True base
  return base

-- | Detects the architecture from the presence of 'syswow64'
detectBottleArch :: FilePath -> IO Arch
detectBottleArch path = do
    let syswow64 = path </> "drive_c" </> "windows" </> "syswow64"
    is64 <- doesDirectoryExist syswow64
    return $ if is64 then Win64 else Win32


-- | Finds a bottle by its name (exact, case-sensitive comparison).
findBottleByName :: T.Text -> [Bottle] -> Maybe Bottle
findBottleByName name bottles = case filter ((== name) . bottleName) bottles of
  (b : _) -> Just b
  []      -> Nothing

-- | Finds the path to a Windows application (.lnk) by its display name, the
-- same one the GUI derives from the file name (see 'Gui.BottleView'). If
-- there are multiple matches (e.g. the same name in different user
-- directories), the first one is returned.
findAppLnkByName :: T.Text -> [FilePath] -> Maybe FilePath
findAppLnkByName name lnkPaths = case filter ((== name) . T.pack . takeBaseName) lnkPaths of
  (p : _) -> Just p
  []      -> Nothing

-- | Scans the directory for existing bottles and detects their architecture
listExistingBottles :: IO [Bottle]
listExistingBottles = do
  base <- getBottlesBaseDir
  exists <- doesDirectoryExist base
  if not exists
    then return []
    else do
      entries <- listDirectory base

      dirs <- filterM (\e -> doesDirectoryExist (base </> e)) entries

      -- Check that 'drive_c' exists (valid prefix)
      validDirs <- filterM (\e -> doesDirectoryExist (base </> e </> "drive_c")) dirs

      forM validDirs $ \name -> do
          let path = base </> name

          maybeConfig <- loadBottleConfig path

          case maybeConfig of
            Just (r, a) -> return $ Bottle (T.pack name) path r a
            Nothing -> do
                -- Fallback for old bottles without a config
                detectedArch <- detectBottleArch path
                return $ Bottle (T.pack name) path SystemWine detectedArch

getAvailableRunners :: IO [RunnerType]
getAvailableRunners = do
  sysWine <- findExecutable "wine"
  let wineList = if isJust sysWine then [SystemWine] else []

  home <- getHomeDirectory
  let compatDir = home </> ".steam/root/compatibilitytools.d"
  
  protonList <- do
    exists <- doesDirectoryExist compatDir
    if exists
      then do
        entries <- listDirectory compatDir
        -- Filter: must be a directory AND contain compatibilitytool.vdf
        paths <- filterM (\e -> do
            let fullPath = compatDir </> e
            isDir <- doesDirectoryExist fullPath
            hasVdf <- doesFileExist (fullPath </> "compatibilitytool.vdf")
            return (isDir && hasVdf)
            ) entries
        return [ Proton (compatDir </> p) | p <- paths ]
      else return []

  return (wineList ++ protonList)

-- | Determines the display name for a runner
getRunnerTypeDisplayName :: RunnerType -> IO T.Text
getRunnerTypeDisplayName SystemWine = do
    (exitCode, out) <- readProcessStdout (proc "wine" ["--version"])
    case exitCode of
        ExitSuccess -> return $ T.strip $ T.pack $ LBS8.unpack out
        _           -> return "System Wine (Unknown Version)"

getRunnerTypeDisplayName (Proton path) = do
    let vdfPath = path </> "compatibilitytool.vdf"
    exists <- doesFileExist vdfPath
    if exists
        then do
            content <- readFile vdfPath
            let name = extractDisplayName (T.pack content)
            if T.null name
                then return $ "Proton (" <> T.pack (takeBaseName path) <> ")" -- Fallback
                else return name
        else return $ "Proton (" <> T.pack (takeBaseName path) <> ")"

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

      saveBottleConfig bottle

      mergedEnv <- getMergedWineEnv bottle

      -- Remove DISPLAY and WAYLAND_DISPLAY from the environment so wineboot
      -- doesn't open a window (like the Gecko/Mono installer dialog).
      let headlessEnv = filter (\(k, _) -> k `notElem` ["DISPLAY", "WAYLAND_DISPLAY"]) mergedEnv

      -- For Proton we also use wineboot (via umu-run wineboot), even though
      -- umu-run often initializes the prefix itself on first start. We call
      -- wineboot regardless for consistency, just adjusting the command.
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

-- | Deletes a bottle and all its snapshots
deleteBottleLogic :: Bottle -> IO ()
deleteBottleLogic bottle@Bottle{..} = do
  putStrLn $ "Starting deletion process for: " ++ T.unpack bottleName

  -- IMPORTANT: stop running processes before deleting files.
  -- This prevents zombie wineservers that would block later tests or re-creation.
  putStrLn "Stopping running processes..."
  _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

  -- 1. Delete all of the bottle's snapshots
  deleteAllSnapshots bottle

  -- 2. Remove the application-menu symlink before the bottle itself
  -- disappears -- so the symlink never points into nothing.
  removeApplicationMenuSymlink bottle

  -- 3. Delete the bottle itself
  putStrLn $ "Deleting Wine prefix: " ++ bottlePath
  isSubvol <- isBtrfsSubvolume bottlePath
  if isSubvol
  then deleteSubvolumeForcible bottlePath
  else removePathForcibly bottlePath

  putStrLn "Deletion completed."

-- Application Menu Integration
--
-- The ".desktop" files are deliberately created *inside* the bottle
-- directory (in "menu/") instead of directly in
-- ~/.local/share/applications: since the bottle directory is its own BTRFS
-- subvolume, they automatically travel along with every snapshot/restore
-- and stay consistent with the actually installed program state. In
-- ~/.local/share/applications there is only a single symlink per bottle,
-- pointing at this "menu" directory (per the Desktop Entry Spec,
-- subdirectories under "applications/" are recursively recognized as
-- desktop file IDs -- Wine's own winemenubuilder does the same).

-- | Directory inside the bottle where the ".desktop" files live.
bottleMenuDir :: Bottle -> FilePath
bottleMenuDir Bottle{..} = bottlePath </> "menu"

-- | Path of an application's ".desktop" file inside the bottle.
desktopFilePath :: Bottle -> T.Text -> FilePath
desktopFilePath bottle appName = bottleMenuDir bottle </> T.unpack appName ++ ".desktop"

-- | Path of an application's (best-effort extracted) icon inside the
-- bottle. Lives in the "menu" directory just like the ".desktop" file
-- itself, so it travels along with snapshots too.
iconFilePath :: Bottle -> T.Text -> FilePath
iconFilePath bottle appName = bottleMenuDir bottle </> "icons" </> T.unpack appName ++ ".png"

-- | Name of the symlink in ~/.local/share/applications for a bottle.
applicationMenuSymlinkName :: Bottle -> String
applicationMenuSymlinkName Bottle{..} = "decanter-" ++ T.unpack bottleName

-- | Escapes an Exec parameter per the Desktop Entry Spec quoting rules
-- (inside double quotes, ", `, $ and \ must be backslash-escaped) and
-- wraps it in double quotes.
quoteExecArg :: T.Text -> T.Text
quoteExecArg arg = "\"" <> T.concatMap escapeChar arg <> "\""
  where
    escapeChar c
      | c `elem` ("\"`$\\" :: String) = T.pack ['\\', c]
      | otherwise = T.singleton c

-- | Ensures ~/.local/share/applications/decanter-<bottle> points at the
-- bottle's "menu" directory. Idempotent.
ensureApplicationMenuSymlink :: Bottle -> IO ()
ensureApplicationMenuSymlink bottle = do
  appsDir <- getXdgDirectory XdgData "applications"
  createDirectoryIfMissing True appsDir
  let linkPath = appsDir </> applicationMenuSymlinkName bottle
  exists <- doesPathExist linkPath
  if exists
    then return ()
    else do
      result <- try (createFileLink (bottleMenuDir bottle) linkPath) :: IO (Either IOException ())
      case result of
        Right () -> return ()
        Left _   -> return () -- race with a concurrent call; the target then already exists

-- | Removes a bottle's application-menu symlink, if present.
removeApplicationMenuSymlink :: Bottle -> IO ()
removeApplicationMenuSymlink bottle = do
  appsDir <- getXdgDirectory XdgData "applications"
  let linkPath = appsDir </> applicationMenuSymlinkName bottle
  exists <- doesPathExist linkPath
  if exists
    then removeFile linkPath
    else return ()

-- | Creates an application-menu entry for a start-menu application. The
-- entry calls "decanter start <bottle> <app>", so it runs through
-- Decanter's own execution logic (env merging, Proton routing, Direct3D
-- wrapper) instead of invoking Wine/the application directly.
--
-- The icon is extracted from the ".lnk" file via 'extractAppIcon' (best
-- effort, see there): if that fails, the entry simply gets no "Icon="
-- field, instead of the whole addition failing.
addToApplicationMenu :: Bottle -> T.Text -> FilePath -> T.Text -> IO ()
addToApplicationMenu bottle appName lnkPath category = do
  createDirectoryIfMissing True (bottleMenuDir bottle)
  createDirectoryIfMissing True (bottleMenuDir bottle </> "icons")
  ensureApplicationMenuSymlink bottle

  let iconPath = iconFilePath bottle appName
  iconExtracted <- extractAppIcon bottle lnkPath iconPath
  let iconLine = if iconExtracted then ["Icon=" <> T.pack iconPath] else []

  writeFile (desktopFilePath bottle appName) $ T.unpack $ T.unlines $
    [ "[Desktop Entry]"
    , "Type=Application"
    , "Name=" <> appName
    , "Exec=decanter start " <> quoteExecArg (bottleName bottle) <> " " <> quoteExecArg appName
    , "Categories=" <> category <> ";"
    , "Terminal=false"
    ] ++ iconLine

-- | Removes a previously created application-menu entry again (including
-- its icon, if one was extracted).
removeFromApplicationMenu :: Bottle -> T.Text -> IO ()
removeFromApplicationMenu bottle appName = do
  let path = desktopFilePath bottle appName
  exists <- doesFileExist path
  if exists
    then removeFile path
    else return ()

  let iconPath = iconFilePath bottle appName
  iconExists <- doesFileExist iconPath
  if iconExists
    then removeFile iconPath
    else return ()

-- | Checks whether an application-menu entry already exists for an application.
isInApplicationMenu :: Bottle -> T.Text -> IO Bool
isInApplicationMenu bottle appName = doesFileExist (desktopFilePath bottle appName)

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
  runSystemTool "xdg-open" [driveC]

runExecutable :: Bottle -> FilePath -> IO ()
runExecutable bottle filePath = do
  let ext = takeExtension filePath
  if ext == ".msi" || ext == ".MSI"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]

runFileWithStart :: Bottle -> FilePath -> IO ()
runFileWithStart bottle path = runCmd bottle "wine" ["start", "/unix", path]

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

-- | Checks whether the system supports 32-bit prefixes.
-- Runs 'WINEARCH=win32 wine --version'; if wine32 is missing, this usually returns ExitCode 1.
checkSystemWine32Support :: IO Bool
checkSystemWine32Support = do
    currentEnv <- getEnvironment
    -- Override WINEARCH, but keep the rest (e.g. PATH)
    let newEnv = ("WINEARCH", "win32") : filter ((/= "WINEARCH") . fst) currentEnv
    
    let procConfig = setEnv newEnv 
                   $ setStderr closed 
                   $ setStdout closed 
                   $ proc "wine" ["--version"]
                   
    result <- runProcess procConfig
    return (result == ExitSuccess)

-- | Returns a list of the architectures supported by the system.
-- Win64 is assumed to always be available.
getSupportedArchitectures :: IO [Arch]
getSupportedArchitectures = do
    win32Support <- checkSystemWine32Support
    if win32Support
       then return [Win64, Win32]
       else return [Win64]
