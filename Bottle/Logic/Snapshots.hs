{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Snapshots
  ( isSnapshotableBottle
  , listSnapshots
  , createSnapshotLogic
  , restoreSnapshotLogic
  , deleteSnapshotLogic
  , openSnapshotFileManager
  , deleteAllSnapshots

    -- * BTRFS Helpers
    -- Only exported because "Bottle.Logic" also needs them to manage a
    -- bottle's own subvolume (which isn't itself a snapshot).
  , isBtrfsSubvolume
  , deleteSubvolumeForcible
  ) where

import Bottle.Types
import Bottle.Logic.Process (killBottleProcesses)
import Logic.SystemTool (runSystemTool)
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    , removePathForcibly
    )
import System.FilePath ((</>))
import Control.Exception (try, IOException, SomeException)
import Control.Monad (forM_)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
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

-- | Restores a bottle from a snapshot
restoreSnapshotLogic :: Bottle -> BottleSnapshot -> IO ()
restoreSnapshotLogic bottle snapshot = do
    putStrLn $ "Restoring bottle '" ++ T.unpack (bottleName bottle) ++ "' from snapshot " ++ show (snapshotId snapshot)

    -- Here too: stop processes safely before touching the filesystem.
    -- killBottleProcesses is now synchronous and waits for completion.
    _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

    deleteSubvolumeForcible (bottlePath bottle)
    Btrfs.snapshot (snapshotPath snapshot) (bottlePath bottle) False
    putStrLn "Restore successful."

-- | Deletes a specific snapshot
deleteSnapshotLogic :: BottleSnapshot -> IO ()
deleteSnapshotLogic snapshot = do
    putStrLn $ "Deleting snapshot: " ++ snapshotPath snapshot
    deleteSubvolumeForcible (snapshotPath snapshot)

-- | Deletes all of a bottle's snapshots along with the (then-empty) snapshot folder.
-- Used by "Bottle.Logic.deleteBottleLogic" when deleting an entire bottle.
deleteAllSnapshots :: Bottle -> IO ()
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
