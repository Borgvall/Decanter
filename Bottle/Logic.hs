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
  , isEngineFamilyChange
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
import Bottle.Logic.Config (saveBottleConfig, loadBottleConfig)
import Bottle.Logic.Process (getMergedWineEnv, killBottleProcesses)
import Bottle.Logic.Snapshots
    ( isBtrfsSubvolume
    , deleteSubvolumeForcible
    , deleteAllSnapshots
    , recoverInterruptedRestores
    , reservedNameSuffixes
    )
import Bottle.Logic.ApplicationMenu (removeApplicationMenuSymlink)
import Bottle.Logic.Direct3dWrappers (isBottleReadyForWindowsApps)
import Bottle.Logic.Runner (engineFamily)
import System.Process.Typed
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , removePathForcibly
    )
import System.FilePath ((</>), takeBaseName)
import Control.Exception (try, throw, IOException, SomeException)
import Control.Monad (filterM, forM)
import Data.List (isSuffixOf)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs

import Logic.Translation (tr)

-- | Changes a bottle's runner, persisting the change via 'saveBottleConfig'
changeBottleRunnerLogic :: Bottle -> RunnerType -> IO Bottle
changeBottleRunnerLogic bottle newRunner = do
  putStrLn $ "Changing runner for bottle '" ++ T.unpack (bottleName bottle)
             ++ "' from " ++ show (runner bottle)
             ++ " to " ++ show newRunner
  let updatedBottle = bottle { runner = newRunner }
  saveBottleConfig updatedBottle
  pure updatedBottle

-- | Whether switching from "oldRunner" to "newRunner" crosses the Wine/Proton
-- engine boundary, as opposed to e.g. switching between two different Proton
-- builds. Wine and Proton set up and maintain a prefix differently enough
-- (Direct3D wrapper DLLs, prefix-creation symlinks into the user's home
-- directory, registry entries) that mixing both on the same, already-
-- initialized prefix leaves it in a state that's hard to reproduce or
-- diagnose -- unlike switching between builds of the same engine, which
-- doesn't have this problem. Used by "Gui.BottleView" to decide whether a
-- runner change needs a confirmation warning.
isEngineFamilyChange :: RunnerType -> RunnerType -> Bool
isEngineFamilyChange oldRunner newRunner =
  engineFamily oldRunner /= engineFamily newRunner

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

-- Matches every constructor explicitly instead of catching the runnable
-- ones with a wildcard: a future runner added to 'RunnerType' should force
-- a decision here rather than silently counting as "ready to run".
blockReason :: Bottle -> IO (Maybe BlockReason)
blockReason bottle = case runner bottle of
  r@MissingSystemWine -> pure (Just (RunnerMissing r))
  r@(MissingProton _) -> pure (Just (RunnerMissing r))
  SystemWine          -> direct3DBlockReason
  Proton _            -> direct3DBlockReason
  where
    direct3DBlockReason = do
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

-- | Scans the directory for existing bottles. First repairs any bottle
-- left half-restored by a crash during 'Bottle.Logic.Snapshots
-- .restoreSnapshotLogic' (see 'recoverInterruptedRestores') -- otherwise a
-- leftover ".restoring" directory (itself a full copy of the bottle,
-- "drive_c" and all) would be picked up below as if it were its own,
-- bogus bottle.
listExistingBottles :: IO [Bottle]
listExistingBottles = do
  base <- getBottlesBaseDir
  exists <- doesDirectoryExist base
  if not exists
    then return []
    else do
      recoverInterruptedRestores base
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
  | ReservedSuffix
  deriving (Show, Eq)

checkNameValidity :: T.Text -> NameValid
checkNameValidity name
  | T.null name = EmptyName
  | T.length name > 256 = NameTooLong
  | T.elem '/' name = ContainsSlash
  -- Reserved so a bottle name can never collide with the temporary
  -- directory restoreSnapshotLogic's crash-safe restore uses (see
  -- Bottle.Logic.Snapshots.reservedNameSuffixes).
  | any (`isSuffixOf` T.unpack name) reservedNameSuffixes = ReservedSuffix
  | otherwise = Valid

explainNameValid :: NameValid -> T.Text
explainNameValid status = case status of
  Valid          -> ""
  EmptyName      -> tr "The name cannot be empty."
  NameTooLong    -> tr "The name is too long (max 256 characters)."
  ContainsSlash  -> tr "The name cannot contain a slash ('/')."
  ReservedSuffix -> tr "The name cannot end with \".restoring\" (reserved for snapshot restore)."

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
      let (bootCmd, bootArgs) = case runner of
            SystemWine        -> ("wineboot", ["-u"])
            Proton _          -> ("umu-run", ["wineboot", "-u"])
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
