{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic
  ( -- * Bottle Management
    listExistingBottles
  , findBottleByName
  , findAppLnkByName
  , createBottleObject
  , createBottleLogic
  , changeBottleRunnerLogic
  , deleteBottleLogic

    -- * Validation
  , checkNameValidity
  , NameValid(Valid)
  , explainNameValid
  ) where

import Bottle.Types
import Bottle.Logic.Process (getMergedWineEnv, killBottleProcesses)
import Bottle.Logic.Snapshots (isBtrfsSubvolume, deleteSubvolumeForcible, deleteAllSnapshots)
import Bottle.Logic.ApplicationMenu (removeApplicationMenuSymlink)
import System.Process.Typed
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , doesFileExist
    , removePathForcibly
    )
import System.FilePath ((</>), takeBaseName)
import Control.Exception (try, IOException, SomeException)
import Control.Monad (filterM, forM)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs

import Logic.Translation (tr)

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
