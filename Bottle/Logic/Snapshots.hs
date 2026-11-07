{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Snapshots
  ( isSnapshotableBottle
  , listSnapshots
  , createSnapshotLogic
  , restoreSnapshotLogic
  , deleteSnapshotLogic
  , openSnapshotFileManager
  , deleteAllSnapshots
  , recoverInterruptedRestores

    -- * Resolving a snapshot reference typed by the user
  , SnapshotRefError(..)
  , resolveSnapshotRef
  , explainSnapshotRefError

    -- * BTRFS Helpers
    -- Only exported because "Bottle.Logic" also needs them to manage a
    -- bottle's own subvolume (which isn't itself a snapshot).
  , isBtrfsSubvolume
  , deleteSubvolumeForcible
  ) where

import Bottle.Types
import Bottle.Logic.Name (ValidName, validNameText, restoreTempSuffix)
import Bottle.Logic.Process (killBottleProcesses)
import Logic.SystemTool (runSystemTool)
import Logic.Translation (tr)
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , removePathForcibly
    , renameDirectory
    )
import System.FilePath ((</>))
import Control.Exception (try, IOException, SomeException)
import Control.Monad (forM_, when)
import Data.List (find, sortOn, isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs
import System.IO.Error

getSnapshotsDir :: IO FilePath
getSnapshotsDir = do
    base <- getXdgDirectory XdgData "Decanter"
    let snapDir = base </> "BottleSnapshots"
    createDirectoryIfMissing True snapDir
    return snapDir

-- | Checks whether a path is a BTRFS subvolume
isBtrfsSubvolume :: FilePath -> IO Bool
isBtrfsSubvolume path = do
    result <- try (Btrfs.getSubvolReadOnly path) :: IO (Either IOException Bool)
    case result of
        Right _ -> return True  -- call succeeded -> it's a subvolume
        Left _  -> return False -- error -> not a subvolume (or FS error)

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
  -- Remove read-only first, otherwise deletion is not allowed
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

isSnapshotableBottle :: BottleG r -> IO Bool
isSnapshotableBottle = isBtrfsSubvolume . bottlePath

listSnapshots :: BottleG r -> IO [BottleSnapshot]
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

-- | Why a snapshot reference didn't resolve to exactly one snapshot.
-- Deliberately not a 'Maybe': "not found" and "ambiguous" call for different
-- advice, and the caller can't recover which one it was from the reference
-- alone.
data SnapshotRefError
  = NoSnapshotWithId Int
  | NoSnapshotWithName T.Text
  | AmbiguousSnapshotName T.Text [Int]
  deriving (Eq, Show)

-- | Resolves a snapshot reference as a user typed it (see
-- "Cli".runRestoreSnapshot / "Cli".runDeleteSnapshot) to one of
-- "snapshots".
--
-- A reference made up entirely of digits always means that snapshot's id;
-- anything else is matched against 'snapshotName'. Ids are the unambiguous
-- handle -- names aren't required to be unique, hence 'AmbiguousSnapshotName'
-- lists every id carrying the given one. An all-digit reference is
-- deliberately *not* retried as a name: a snapshot merely named "7" must
-- never be restored by accident just because no id 7 exists.
resolveSnapshotRef :: T.Text -> [BottleSnapshot] -> Either SnapshotRefError BottleSnapshot
resolveSnapshotRef ref snapshots
  | Just refId <- readMaybe (T.unpack ref) =
      case find ((== refId) . snapshotId) snapshots of
        Just snapshot -> Right snapshot
        Nothing       -> Left (NoSnapshotWithId refId)
  | otherwise =
      case filter ((== ref) . snapshotName) snapshots of
        []         -> Left (NoSnapshotWithName ref)
        [snapshot] -> Right snapshot
        found      -> Left (AmbiguousSnapshotName ref (map snapshotId found))

explainSnapshotRefError :: SnapshotRefError -> T.Text
explainSnapshotRefError (NoSnapshotWithId refId) = T.concat
  [ tr "No snapshot with id ", T.pack (show refId), tr " in this bottle." ]
explainSnapshotRefError (NoSnapshotWithName refName) = T.concat
  [ tr "No snapshot named '", refName, tr "' in this bottle." ]
explainSnapshotRefError (AmbiguousSnapshotName refName refIds) = T.concat
  [ tr "Several snapshots are named '"
  , refName
  , tr "' (ids: "
  , T.intercalate ", " (map (T.pack . show) refIds)
  , tr "). Use the id instead."
  ]

-- | Creates a snapshot of "bottle" and returns the snapshot it created.
--
-- Returns it rather than '()' because the id is derived here, from the
-- bottle's existing snapshots: a caller that wants to report or address the
-- new snapshot would otherwise have to list the bottle's snapshots again and
-- guess which of them is the one it just made.
createSnapshotLogic :: BottleG r -> ValidName -> IO BottleSnapshot
createSnapshotLogic bottle sName = do
    baseSnapDir <- getSnapshotsDir
    let bottleSnapDir = baseSnapDir </> T.unpack (bottleName bottle)
    createDirectoryIfMissing True bottleSnapDir

    currentSnaps <- listSnapshots bottle
    let nextId = getNextSnapshotId currentSnaps

    let folderName = show nextId ++ "_" ++ T.unpack (validNameText sName)
    let destPath = bottleSnapDir </> folderName

    Btrfs.snapshot (bottlePath bottle) destPath True

    pure $ BottleSnapshot nextId (validNameText sName) destPath

-- | Restores a bottle from a snapshot.
--
-- Builds the restored copy at a temporary path first, only deleting the
-- live bottle directory once that copy is fully built and ready, then
-- moves it into place with a single rename -- unlike the previous
-- delete-then-recreate approach, where a crash between deleting the old
-- bottle and creating the new one left the bottle permanently invisible
-- (not listed, snapshots unreachable via the GUI) even though no data was
-- actually lost. Any crash during the (much smaller) remaining window
-- between the delete and the rename is fully recovered by
-- 'recoverInterruptedRestores' on the next 'Bottle.Logic.listExistingBottles'
-- -- the fully-built copy is never touched until it's confirmed ready, so
-- recovery never needs to redo the actual restore.
restoreSnapshotLogic :: Bottle -> BottleSnapshot -> IO ()
restoreSnapshotLogic bottle snapshot = do
    putStrLn $ "Restoring bottle '" ++ T.unpack (bottleName bottle) ++ "' from snapshot " ++ show (snapshotId snapshot)

    -- Here too: stop processes safely before touching the filesystem.
    -- killBottleProcesses is now synchronous and waits for completion.
    _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

    let restoringPath = bottlePath bottle ++ restoreTempSuffix

    Btrfs.snapshot (snapshotPath snapshot) restoringPath False

    deleteSubvolumeForcible (bottlePath bottle)
    renameDirectory restoringPath (bottlePath bottle)

    putStrLn "Restore successful."

-- | Repairs a bottle left in a half-restored state by a crash during
-- 'restoreSnapshotLogic' (see there). Safe and idempotent to call even
-- when nothing needs repairing, which is the common case -- meant to be
-- called once before listing bottles, so a bottle is never permanently
-- invisible just because Decanter happened to die mid-restore.
recoverInterruptedRestores :: FilePath -> IO ()
recoverInterruptedRestores base = do
  entries <- listDirectory base

  -- A ".restoring" directory means the new, fully-built copy is ready but
  -- wasn't swapped in yet -- finish the job: discard whatever's left of
  -- the old bottle (if the crash happened before that already ran; a
  -- no-op otherwise) and move the restored copy into place.
  forM_ [ e | e <- entries, restoreTempSuffix `isSuffixOf` e ] $ \restoringName -> do
    let name          = take (length restoringName - length restoreTempSuffix) restoringName
        restoringPath = base </> restoringName
        finalPath     = base </> name
    finalExists <- doesDirectoryExist finalPath
    when finalExists $ deleteSubvolumeForcible finalPath
    renameDirectory restoringPath finalPath

-- | Deletes a specific snapshot
deleteSnapshotLogic :: BottleSnapshot -> IO ()
deleteSnapshotLogic snapshot = do
    putStrLn $ "Deleting snapshot: " ++ snapshotPath snapshot
    deleteSubvolumeForcible (snapshotPath snapshot)

-- | Deletes all of a bottle's snapshots along with the (then-empty) snapshot folder.
-- Used by "Bottle.Logic.deleteBottleLogic" when deleting an entire bottle.
deleteAllSnapshots :: BottleG r -> IO ()
deleteAllSnapshots bottle = do
    baseSnapDir <- getSnapshotsDir
    let bottleSnapDir = baseSnapDir </> T.unpack (bottleName bottle)

    snaps <- listSnapshots bottle
    forM_ snaps $ \s -> deleteSubvolumeForcible (snapshotPath s)

    removePathForcibly bottleSnapDir

-- | Opens the file manager in the snapshot's drive_c
openSnapshotFileManager :: BottleSnapshot -> IO ()
openSnapshotFileManager snapshot = do
    let driveC = snapshotPath snapshot </> "drive_c"
    runSystemTool "xdg-open" [driveC]
