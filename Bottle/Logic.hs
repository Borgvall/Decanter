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

    -- * Readiness
  , BlockReason(..)
  , blockReason
  , explainBlockReason

    -- * Validation
  , checkNameValidity
  , NameValid(Valid)
  , explainNameValid
  ) where

import Bottle.Types
import Bottle.Logic.Process (getMergedWineEnv, killBottleProcesses)
import Bottle.Logic.Snapshots (isBtrfsSubvolume, deleteSubvolumeForcible, deleteAllSnapshots)
import Bottle.Logic.ApplicationMenu (removeApplicationMenuSymlink)
import Bottle.Logic.Direct3dWrappers (isBottleReadyForWindowsApps)
import Bottle.Logic.Runner (findProtonPathByName)
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
    )
import System.FilePath ((</>), takeBaseName)
import Control.Exception (try, throw, IOException, SomeException)
import Control.Monad (filterM, forM)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs

import Logic.Translation (tr)

-- | Path to a bottle's configuration file
getConfigPath :: FilePath -> FilePath
getConfigPath bottleDir = bottleDir </> "decanter.cfg"

-- | On-disk shape for a bottle's runner -- deliberately not 'RunnerType'
-- itself. A Proton build is identified by its *name* (the compatibility
-- tool directory's basename, e.g. "GE-Proton10-25") rather than its path:
-- unlike a path, a name survives the tool moving between the multiple
-- directories "Bottle.Logic.Runner" now searches (a system package update,
-- a Nix rebuild, or Steam's own precedence changing which directory wins),
-- mirroring how Steam's own "compatibilitytool.vdf" identifies tools by
-- name too. Never itself holds "missing" state -- see 'resolvePersistedRunner'.
data PersistedRunner = PersistedSystemWine | PersistedProtonName T.Text
  deriving (Show, Read)

toPersistedRunner :: RunnerType -> PersistedRunner
toPersistedRunner SystemWine        = PersistedSystemWine
toPersistedRunner (Proton p)        = PersistedProtonName (T.pack (takeBaseName p))
toPersistedRunner MissingSystemWine = PersistedSystemWine
toPersistedRunner (MissingProton p) = PersistedProtonName (T.pack (takeBaseName p))

-- | Saves the bottle's configuration (runner)
saveBottleConfig :: Bottle -> IO ()
saveBottleConfig b = do
    let content = show (toPersistedRunner (runner b))
    writeFile (getConfigPath (bottlePath b)) content

-- | Stand-in for the 'Arch' field pre-existing config files (written before
-- 32-bit prefix support was removed) still have, so 'loadBottleConfig' can
-- parse them; the actual value is discarded.
data LegacyArch = Win32 | Win64 deriving (Read)

-- | Loads the bottle's configuration. Understands the current
-- 'PersistedRunner' format, the previous format (a bare 'RunnerType',
-- storing a Proton path directly), and the legacy
-- '(RunnerType, LegacyArch)' tuple format from before 32-bit prefix support
-- was removed -- so bottles created with any older Decanter version don't
-- lose their configured runner. Only 'saveBottleConfig' ever writes the
-- current format; existing files stay in whichever older format they're
-- already in until the next save (e.g. a runner change).
loadBottleConfig :: FilePath -> IO (Maybe RunnerType)
loadBottleConfig bottleDir = do
    let path = getConfigPath bottleDir
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            -- Plain 'reads' for safe parsing
            case reads content :: [(PersistedRunner, String)] of
                [(pr, _)] -> Just <$> resolvePersistedRunner pr
                _ -> case reads content of
                    [(r, _)] -> Just <$> resolveRunnerAvailability r
                    _ -> case reads content :: [((RunnerType, LegacyArch), String)] of
                        [((r, _), _)] -> Just <$> resolveRunnerAvailability r
                        _ -> do
                            putStrLn $ "Could not parse: " ++ path
                            return Nothing
        else return Nothing

-- | Resolves a freshly parsed 'PersistedRunner' to a 'RunnerType', looking
-- up a Proton name's current path fresh on every load (see
-- 'Bottle.Logic.Runner.findProtonPathByName') -- downgrading to
-- 'MissingProton' (keeping just the name, for display) if no tool by that
-- name is currently found anywhere.
resolvePersistedRunner :: PersistedRunner -> IO RunnerType
resolvePersistedRunner PersistedSystemWine = do
    available <- isJust <$> findExecutable "wine"
    pure $ if available then SystemWine else MissingSystemWine
resolvePersistedRunner (PersistedProtonName name) = do
    mPath <- findProtonPathByName name
    pure $ case mPath of
        Just path -> Proton path
        Nothing   -> MissingProton (T.unpack name)

-- | Re-checks a freshly parsed (previous-format) runner's availability by
-- its literal persisted path, downgrading it to
-- 'MissingSystemWine'/'MissingProton' if that exact path can no longer be
-- found. Deliberately never persisted -- always recomputed on load, since
-- availability can change between runs independently of the bottle's own
-- configuration. Only used for the previous, path-based format; the
-- current format resolves by name instead (see 'resolvePersistedRunner').
resolveRunnerAvailability :: RunnerType -> IO RunnerType
resolveRunnerAvailability SystemWine = do
    available <- isJust <$> findExecutable "wine"
    pure $ if available then SystemWine else MissingSystemWine
resolveRunnerAvailability (Proton p) = do
    available <- doesFileExist (p </> "compatibilitytool.vdf")
    pure $ if available then Proton p else MissingProton p
-- Never actually persisted (see 'saveBottleConfig'), kept only so this
-- function is total.
resolveRunnerAvailability alreadyMissing = pure alreadyMissing

-- | Changes a bottle's runner (bottle object only, does not save)
changeBottleRunnerLogic :: Bottle -> RunnerType -> IO Bottle
changeBottleRunnerLogic bottle newRunner = do
  putStrLn $ "Changing runner for bottle '" ++ T.unpack (bottleName bottle)
             ++ "' from " ++ show (runner bottle)
             ++ " to " ++ show newRunner
  let updatedBottle = bottle { runner = newRunner }
  saveBottleConfig updatedBottle
  pure updatedBottle

-- | Why a bottle currently can't run Windows programs, if at all --
-- 'Nothing' means it's ready. This is the single check both the GUI
-- (to grey out/explain the relevant buttons) and 'decanter start'/
-- 'decanter open' (to fail up front with a clear message, instead of
-- silently doing nothing deep inside "Bottle.Logic.Programs".runCmd)
-- should use, rather than separately querying runner/Direct3D internals.
data BlockReason
  = RunnerMissing RunnerType
  | Direct3DWrapperDangling
  deriving (Eq, Show)

explainBlockReason :: BlockReason -> T.Text
explainBlockReason (RunnerMissing MissingSystemWine) =
  tr "System Wine was not found. Install it, or change the runner."
explainBlockReason (RunnerMissing (MissingProton path)) = T.concat
  [ tr "Proton build '", T.pack (takeBaseName path), tr "' was not found. Change the runner, or reinstall it." ]
-- Unreachable (RunnerMissing is only ever constructed with a Missing*
-- runner, see 'blockReason'), kept only so this function is total.
explainBlockReason (RunnerMissing _) =
  tr "The configured runner was not found."
explainBlockReason Direct3DWrapperDangling =
  tr "The Direct3D wrapper's files are missing (e.g. removed by Nix garbage collection). Windows programs are blocked until this is repaired."

blockReason :: Bottle -> IO (Maybe BlockReason)
blockReason bottle = case runner bottle of
  r@MissingSystemWine -> pure (Just (RunnerMissing r))
  r@(MissingProton _) -> pure (Just (RunnerMissing r))
  _ -> do
    ready <- isBottleReadyForWindowsApps bottle
    pure $ if ready then Nothing else Just Direct3DWrapperDangling

-- | Determines the base directory for all bottles
getBottlesBaseDir :: IO FilePath
getBottlesBaseDir = do
  base <- getXdgDirectory XdgData "Decanter"
  createDirectoryIfMissing True base
  return base

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

-- | Scans the directory for existing bottles
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
            Just r  -> return $ Bottle (T.pack name) path r
            -- Fallback for old bottles without a config
            Nothing -> return $ Bottle (T.pack name) path SystemWine

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

createBottleObject :: T.Text -> RunnerType -> IO Bottle
createBottleObject name rType = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack name
  return $ Bottle name path rType

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
      --
      -- MissingSystemWine/MissingProton can't actually occur here: "runner"
      -- was just freshly chosen from 'createBottleObject' (fed by currently
      -- available runners), never loaded from a config file. Handled only
      -- for exhaustiveness, per Bottle.Types.RunnerMissingError.
      let bootCmd = case runner of
            SystemWine        -> "wineboot"
            Proton _          -> "umu-run"
            MissingSystemWine -> throw (RunnerMissingError runner)
            MissingProton _   -> throw (RunnerMissingError runner)

      let bootArgs = case runner of
            SystemWine        -> ["-u"]
            Proton _          -> ["wineboot", "-u"]
            MissingSystemWine -> throw (RunnerMissingError runner)
            MissingProton _   -> throw (RunnerMissingError runner)

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
