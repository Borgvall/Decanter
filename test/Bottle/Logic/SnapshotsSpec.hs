{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.SnapshotsSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Logic.Snapshots
import Bottle.Logic.TestSupport (withTestBottle, testName)
import Bottle.Logic.Process (killBottleProcesses)
import Bottle.Types
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  , doesFileExist
  , doesDirectoryExist
  )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>), takeDirectory)
import Control.Exception (finally, try, SomeException)
import Data.List (find)
import qualified System.Linux.Btrfs as Btrfs

-- | Sets up an isolated test environment
withTestEnvironment :: IO () -> IO ()
withTestEnvironment action = do
    cwd <- getCurrentDirectory
    let testDir = cwd </> "test-env"
    let xdgDataHome = testDir </> ".local" </> "share"

    createDirectoryIfMissing True xdgDataHome

    -- Redirect XDG_DATA_HOME so Decanter writes into our test folder
    setEnv "XDG_DATA_HOME" xdgDataHome

    action `finally` do
        removePathForcibly testDir
        unsetEnv "XDG_DATA_HOME"

spec :: Spec
spec = do
  describe "Bottle.Logic.Snapshots" $ around_ withTestEnvironment $ do

    it "handles snapshots if supported" $ do
      withTestBottle "SnapshotTestBottle" SystemWine $ \bottle -> do
        supportsSnaps <- isSnapshotableBottle bottle

        if supportsSnaps
          then do
            createSnapshotLogic bottle (testName "Initial")
            snaps1 <- listSnapshots bottle
            case snaps1 of
              [snap1] -> snapshotName snap1 `shouldBe` "Initial"
              _ -> expectationFailure $ "Expected exactly one snapshot, got: " ++ show snaps1

            let testFile = bottlePath bottle </> "testfile.txt"
            writeFile testFile "State 2: With File"
            existsAfterWrite <- doesFileExist testFile
            existsAfterWrite `shouldBe` True

            createSnapshotLogic bottle (testName "WithFile")
            snaps2 <- listSnapshots bottle
            length snaps2 `shouldBe` 2

            case find (\s -> snapshotName s == "Initial") snaps2 of
              Nothing -> expectationFailure $ "Expected to find a snapshot named 'Initial', got: " ++ show snaps2
              Just snapInitial -> do
                restoreSnapshotLogic (widenRunner bottle) snapInitial

                -- The file must be gone after restoring the snapshot taken before it existed
                existsAfterRestore1 <- doesFileExist testFile
                existsAfterRestore1 `shouldBe` False

                deleteSnapshotLogic snapInitial
                snaps3 <- listSnapshots bottle
                case snaps3 of
                  [snap3] -> snapshotName snap3 `shouldBe` "WithFile"
                  _ -> expectationFailure $ "Expected exactly one snapshot, got: " ++ show snaps3

                case find (\s -> snapshotName s == "WithFile") snaps3 of
                  Nothing -> expectationFailure $ "Expected to find a snapshot named 'WithFile', got: " ++ show snaps3
                  Just snapWithFile -> do
                    restoreSnapshotLogic (widenRunner bottle) snapWithFile

                    -- The file must be back after restoring the snapshot taken after it was written
                    existsAfterRestore2 <- doesFileExist testFile
                    existsAfterRestore2 `shouldBe` True
                    content <- readFile testFile
                    content `shouldBe` "State 2: With File"

                    -- Exercises deleteBottleLogic's interaction with
                    -- listExistingBottles directly (on top of the automatic
                    -- cleanup 'withTestBottle' does afterwards regardless).
                    deleteBottleLogic (widenRunner bottle)
                    remainingBottles <- listExistingBottles
                    let ourBottles = filter (\b -> bottleName b == "SnapshotTestBottle") remainingBottles
                    ourBottles `shouldBe` []

          else do
            -- Without snapshot support, the list should at least be empty and retrievable
            snaps <- listSnapshots bottle
            snaps `shouldBe` []
            pendingWith "No BTRFS detected; snapshot integration test needs a real BTRFS filesystem."

    it "deleteAllSnapshots removes every snapshot and the snapshot directory" $ do
      withTestBottle "DeleteAllSnapshotsTestBottle" SystemWine $ \bottle -> do
        supportsSnaps <- isSnapshotableBottle bottle

        if supportsSnaps
          then do
            createSnapshotLogic bottle (testName "First")
            createSnapshotLogic bottle (testName "Second")
            snapsBefore <- listSnapshots bottle
            length snapsBefore `shouldBe` 2

            deleteAllSnapshots bottle
            snapsAfter <- listSnapshots bottle
            snapsAfter `shouldBe` []
          else
            pendingWith "No BTRFS detected; deleteAllSnapshots test needs a real BTRFS filesystem."

    it "recoverInterruptedRestores finishes a restore interrupted before the old bottle was deleted" $ do
      withTestBottle "InterruptedRestoreBeforeDeleteTest" SystemWine $ \bottle -> do
        supportsSnaps <- isSnapshotableBottle bottle

        if supportsSnaps
          then do
            -- As restoreSnapshotLogic itself does: stop processes before
            -- touching the filesystem, so a still-resident wineserver from
            -- createBottleLogic's wineboot doesn't race with the delete/
            -- snapshot calls below.
            _ <- try (killBottleProcesses (widenRunner bottle)) :: IO (Either SomeException ())

            -- Simulate a crash right after "Btrfs.snapshot ... restoringPath"
            -- succeeded but before the old bottle was deleted: the live
            -- bottle is still under its own name, with a full ".restoring"
            -- copy next to it.
            let restoringPath = bottlePath bottle ++ ".restoring"
            Btrfs.snapshot (bottlePath bottle) restoringPath False

            recoverInterruptedRestores (takeDirectory (bottlePath bottle))

            doesDirectoryExist (bottlePath bottle) `shouldReturn` True
            doesDirectoryExist restoringPath `shouldReturn` False
          else
            pendingWith "No BTRFS detected; recoverInterruptedRestores integration test needs a real BTRFS filesystem."

    it "recoverInterruptedRestores finishes a restore interrupted after the old bottle was deleted" $ do
      withTestBottle "InterruptedRestoreAfterDeleteTest" SystemWine $ \bottle -> do
        supportsSnaps <- isSnapshotableBottle bottle

        if supportsSnaps
          then do
            _ <- try (killBottleProcesses (widenRunner bottle)) :: IO (Either SomeException ())

            -- Simulate a crash between the delete and the final rename: the
            -- old bottle is already gone, only the fully-built ".restoring"
            -- copy is left, and nothing exists under the bottle's real name
            -- -- the exact state that used to make a bottle vanish for good.
            let restoringPath = bottlePath bottle ++ ".restoring"
            Btrfs.snapshot (bottlePath bottle) restoringPath False
            deleteSubvolumeForcible (bottlePath bottle)
            doesDirectoryExist (bottlePath bottle) `shouldReturn` False

            recoverInterruptedRestores (takeDirectory (bottlePath bottle))

            doesDirectoryExist (bottlePath bottle) `shouldReturn` True
            doesDirectoryExist restoringPath `shouldReturn` False
          else
            pendingWith "No BTRFS detected; recoverInterruptedRestores integration test needs a real BTRFS filesystem."

    it "isBtrfsSubvolume returns False for a plain (non-BTRFS) directory" $ do
      cwd <- getCurrentDirectory
      let plainDir = cwd </> "test-env" </> "not-a-subvolume"
      createDirectoryIfMissing True plainDir
      isBtrfsSubvolume plainDir `shouldReturn` False

    it "deleteSubvolumeForcible is exercised end-to-end by the snapshot lifecycle tests above" $ do
      pendingWith "Covered indirectly by 'handles snapshots if supported' and 'deleteAllSnapshots ...' whenever BTRFS is available; not safely unit-testable without a real subvolume."

    it "opens the snapshot's drive_c in the file manager" $ do
      pendingWith "openSnapshotFileManager launches an external GUI app (xdg-open) and isn't unit-testable in CI."
