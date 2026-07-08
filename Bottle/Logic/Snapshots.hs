{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Snapshots
  ( isSnapshotableBottle
  , listSnapshots
  , createSnapshotLogic
  , restoreSnapshotLogic
  , deleteSnapshotLogic
  , openSnapshotFileManager
  ) where

import Bottle.Types
import Bottle.Logic (isBtrfsSubvolume, deleteSubvolumeForcible, runSystemTool, killBottleProcesses)
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , listDirectory
    , doesDirectoryExist
    )
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified System.Linux.Btrfs as Btrfs

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
