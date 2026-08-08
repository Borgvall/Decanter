{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic
  ( -- * Bottle Management
    listExistingBottles
  , findBottleByName
  , findAppLnkByName
  , createBottleLogic
  , changeBottleRunnerLogic
  , isEngineFamilyChange
  , deleteBottleLogic

    -- * Readiness
  , BlockReason(..)
  , launchableBottle
  , explainBlockReason
  ) where

import Bottle.Types
import Bottle.Logic.Config (saveBottleConfig, loadBottleConfig)
import Bottle.Logic.Process (getMergedWineEnv, killBottleProcesses)
import Bottle.Logic.Snapshots
    ( isBtrfsSubvolume
    , deleteSubvolumeForcible
    , deleteAllSnapshots
    , recoverInterruptedRestores
    )
import Bottle.Logic.Name (ValidName, validNameText)
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
import Control.Exception (try, throwIO, IOException, SomeException)
import Control.Monad (filterM, forM, when)
import System.IO.Error (mkIOError, alreadyExistsErrorType)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs

import Logic.Translation (tr)

-- | Changes a bottle's runner, persisting the change via 'saveBottleConfig'.
-- Takes an 'ExistingRunner': a runner is only ever switched to one the
-- picker offered, and those come from 'getAvailableRunners'.
changeBottleRunnerLogic :: Bottle -> ExistingRunner -> IO Bottle
changeBottleRunnerLogic bottle newRunner = do
  putStrLn $ "Changing runner for bottle '" ++ T.unpack (bottleName bottle)
             ++ "' from " ++ show (runner bottle)
             ++ " to " ++ show newRunner
  let updatedBottle = bottle { runner = Existing newRunner }
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

-- | Why a bottle currently can't run Windows programs. See
-- 'launchableRunner', which is where one of these comes from.
data BlockReason
  = RunnerMissing MissingRunner
  | Direct3DWrapperDangling
  deriving (Eq, Show)

explainBlockReason :: BlockReason -> T.Text
explainBlockReason (RunnerMissing MissingSystemWine) =
  tr "System Wine was not found. Install it, or change the runner."
explainBlockReason (RunnerMissing (MissingProton path)) = T.concat
  [ tr "Proton build '", T.pack (takeBaseName path), tr "' was not found. Change the runner, or reinstall it." ]
explainBlockReason Direct3DWrapperDangling =
  tr "The Direct3D wrapper's files are missing (e.g. removed by Nix garbage collection). Windows programs are blocked until this is repaired."

-- | The bottle as something that may currently launch Windows programs, or
-- why it can't. This is the single check both the GUI (to grey out/explain
-- the relevant widgets) and 'decanter start'/'decanter open' (to fail up
-- front with a clear message, instead of silently doing nothing deep inside
-- "Bottle.Logic.Programs".runCmd) should use, rather than separately
-- querying runner/Direct3D internals.
--
-- Returns the bottle at @BottleG ExistingRunner@ rather than just "yes,
-- ready": passing this check is exactly what entitles a caller to start
-- something, so handing back what to start it /with/ means no call site has
-- to ask a second time, and none of them needs a branch for a case the
-- check already ruled out. Handing back the whole bottle rather than the
-- bare runner additionally keeps the two from being carried around as a
-- pair that nothing forces to agree.
--
-- This is the only place that narrows a bottle's runner. Note that the
-- narrowed type proves only that the runner is installed -- 'Right' here
-- means more than its type does, because 'Direct3DWrapperDangling' is
-- checked at the same time but recorded nowhere.
launchableBottle :: Bottle -> IO (Either BlockReason (BottleG ExistingRunner))
launchableBottle bottle = case runner bottle of
  Missing m  -> pure (Left (RunnerMissing m))
  Existing r -> do
    ready <- isBottleReadyForWindowsApps bottle
    pure $ if ready then Right (bottle { runner = r }) else Left Direct3DWrapperDangling

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
            Nothing -> return $ Bottle (T.pack name) path (Existing SystemWine)

createVolume :: FilePath -> IO ()
createVolume path = do
  result <- try (Btrfs.createSubvol path) :: IO (Either IOException ())
  case result of
    Right _ -> putStrLn $ "BTRFS subvolume created: " ++ path
    Left _  -> do
      putStrLn "No BTRFS or error, using standard directory."
      createDirectoryIfMissing True path

-- | Creates a bottle: its volume, its config file, and an initialized Wine
-- prefix, and returns the 'Bottle' it created.
--
-- Takes a 'ValidName' and an 'ExistingRunner' rather than a finished
-- 'Bottle', so neither an unusable name nor a runner that isn't installed
-- can reach it -- both are exactly what initializing a prefix needs to be
-- settled beforehand, and the function has a branch for neither. The
-- bottle record only comes into existence here, as a *result* of the
-- creation, so callers that need it afterwards (to delete it again, to
-- compare it against 'listExistingBottles', ...) get the one that was
-- actually created rather than rebuilding their own.
--
-- Returned at @BottleG ExistingRunner@, the same type it was created at:
-- narrowing it again afterwards would mean recomputing what this function
-- already knew. Callers that need the wider type apply 'widenRunner'.
--
-- Refuses a name an existing bottle already occupies. 'ValidName' can't
-- rule this out -- the naming rules are pure text rules and know nothing
-- about what's on disk -- and without the check the creation quietly
-- proceeds *into* the existing bottle: 'createVolume' falls back to the
-- directory that's already there, 'saveBottleConfig' overwrites its
-- decanter.cfg (and with it the runner it was created with), and wineboot
-- re-runs over its prefix.
createBottleLogic :: ValidName -> ExistingRunner -> IO (BottleG ExistingRunner)
createBottleLogic name existingRunner = do
  base <- getBottlesBaseDir
  let path = base </> T.unpack (validNameText name)
      bottle = Bottle (validNameText name) path existingRunner

  taken <- doesDirectoryExist path
  when taken $ throwIO $
    mkIOError alreadyExistsErrorType "createBottleLogic" Nothing (Just path)

  createVolume (bottlePath bottle)

  saveBottleConfig (widenRunner bottle)

  mergedEnv <- getMergedWineEnv bottle

  -- Remove DISPLAY and WAYLAND_DISPLAY from the environment so wineboot
  -- doesn't open a window (like the Gecko/Mono installer dialog).
  let headlessEnv = filter (\(k, _) -> k `notElem` ["DISPLAY", "WAYLAND_DISPLAY"]) mergedEnv

  -- For Proton we also use wineboot (via umu-run wineboot), even though
  -- umu-run often initializes the prefix itself on first start. We call
  -- wineboot regardless for consistency, just adjusting the command.
  let (bootCmd, bootArgs) = case existingRunner of
        SystemWine -> ("wineboot", ["-u"])
        Proton _   -> ("umu-run", ["wineboot", "-u"])

  runProcess_ $ setEnv headlessEnv $ proc bootCmd bootArgs

  return bottle

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
