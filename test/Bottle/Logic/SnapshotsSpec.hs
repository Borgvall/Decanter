{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.SnapshotsSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Logic.Snapshots
import Bottle.Logic.Process (killBottleProcesses)
import Bottle.Types
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  , doesFileExist
  , doesDirectoryExist
  , renameDirectory
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
      bottle <- createBottleObject "SnapshotTestBottle" SystemWine

      -- Creates the bottle (this runs wineboot, if Wine is installed)
      createBottleLogic bottle

      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          createSnapshotLogic bottle "Initial"
          snaps1 <- listSnapshots bottle
          case snaps1 of
            [snap1] -> snapshotName snap1 `shouldBe` "Initial"
            _ -> expectationFailure $ "Expected exactly one snapshot, got: " ++ show snaps1

          let testFile = bottlePath bottle </> "testfile.txt"
          writeFile testFile "State 2: With File"
          existsAfterWrite <- doesFileExist testFile
          existsAfterWrite `shouldBe` True

          createSnapshotLogic bottle "WithFile"
          snaps2 <- listSnapshots bottle
          length snaps2 `shouldBe` 2

          case find (\s -> snapshotName s == "Initial") snaps2 of
            Nothing -> expectationFailure $ "Expected to find a snapshot named 'Initial', got: " ++ show snaps2
            Just snapInitial -> do
              restoreSnapshotLogic bottle snapInitial

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
                  restoreSnapshotLogic bottle snapWithFile

                  -- The file must be back after restoring the snapshot taken after it was written
                  existsAfterRestore2 <- doesFileExist testFile
                  existsAfterRestore2 `shouldBe` True
                  content <- readFile testFile
                  content `shouldBe` "State 2: With File"

                  deleteBottleLogic bottle

                  remainingBottles <- listExistingBottles
                  let ourBottles = filter (\b -> bottleName b == "SnapshotTestBottle") remainingBottles
                  ourBottles `shouldBe` []

        else do
          putStrLn "Skipping snapshot integration tests (no BTRFS detected)"
          -- Without snapshot support, the list should at least be empty and retrievable
          snaps <- listSnapshots bottle
          snaps `shouldBe` []

          deleteBottleLogic bottle

    it "deleteAllSnapshots removes every snapshot and the snapshot directory" $ do
      bottle <- createBottleObject "DeleteAllSnapshotsTestBottle" SystemWine
      createBottleLogic bottle

      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          createSnapshotLogic bottle "First"
          createSnapshotLogic bottle "Second"
          snapsBefore <- listSnapshots bottle
          length snapsBefore `shouldBe` 2

          deleteAllSnapshots bottle
          snapsAfter <- listSnapshots bottle
          snapsAfter `shouldBe` []
        else
          putStrLn "Skipping deleteAllSnapshots test (no BTRFS detected)"

      deleteBottleLogic bottle

    it "recoverInterruptedRestores finishes a swap interrupted before the old bottle was moved aside" $ do
      bottle <- createBottleObject "InterruptedRestoreBeforeSwapTest" SystemWine
      createBottleLogic bottle
      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          -- As restoreSnapshotLogic itself does: stop processes before
          -- touching the filesystem, so a still-resident wineserver from
          -- createBottleLogic's wineboot doesn't race with the rename/
          -- snapshot calls below.
          _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

          -- Simulate a crash right after "Btrfs.snapshot ... restoringPath"
          -- succeeded but before either rename ran: the live bottle is
          -- still under its own name, with a full ".restoring" copy next
          -- to it.
          let restoringPath = bottlePath bottle ++ ".restoring"
              backupPath    = bottlePath bottle ++ ".pre-restore"
          Btrfs.snapshot (bottlePath bottle) restoringPath False

          recoverInterruptedRestores (takeDirectory (bottlePath bottle))

          doesDirectoryExist (bottlePath bottle) `shouldReturn` True
          doesDirectoryExist restoringPath `shouldReturn` False
          doesDirectoryExist backupPath `shouldReturn` False
        else
          putStrLn "Skipping recoverInterruptedRestores test (no BTRFS detected)"

      deleteBottleLogic bottle

    it "recoverInterruptedRestores finishes a swap interrupted after the old bottle was moved aside" $ do
      bottle <- createBottleObject "InterruptedRestoreAfterSwapTest" SystemWine
      createBottleLogic bottle
      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

          -- Simulate a crash between the two renames: the old bottle is
          -- already under ".pre-restore", the new copy is still under
          -- ".restoring", and nothing exists under the bottle's real name
          -- -- the exact state that used to make a bottle vanish for good.
          let restoringPath = bottlePath bottle ++ ".restoring"
              backupPath    = bottlePath bottle ++ ".pre-restore"
          Btrfs.snapshot (bottlePath bottle) restoringPath False
          renameDirectory (bottlePath bottle) backupPath
          doesDirectoryExist (bottlePath bottle) `shouldReturn` False

          recoverInterruptedRestores (takeDirectory (bottlePath bottle))

          doesDirectoryExist (bottlePath bottle) `shouldReturn` True
          doesDirectoryExist restoringPath `shouldReturn` False
          doesDirectoryExist backupPath `shouldReturn` False
        else
          putStrLn "Skipping recoverInterruptedRestores test (no BTRFS detected)"

      deleteBottleLogic bottle

    it "recoverInterruptedRestores cleans up an orphaned pre-restore backup left after a completed swap" $ do
      bottle <- createBottleObject "OrphanedBackupTest" SystemWine
      createBottleLogic bottle
      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())

          -- Simulate a crash after both renames succeeded but before the
          -- backup's cleanup ran: the bottle is already fully restored
          -- under its real name, only the stale backup is left over.
          let backupPath = bottlePath bottle ++ ".pre-restore"
          Btrfs.snapshot (bottlePath bottle) backupPath False

          recoverInterruptedRestores (takeDirectory (bottlePath bottle))

          doesDirectoryExist (bottlePath bottle) `shouldReturn` True
          doesDirectoryExist backupPath `shouldReturn` False
        else
          putStrLn "Skipping recoverInterruptedRestores test (no BTRFS detected)"

      deleteBottleLogic bottle

    it "isBtrfsSubvolume returns False for a plain (non-BTRFS) directory" $ do
      cwd <- getCurrentDirectory
      let plainDir = cwd </> "test-env" </> "not-a-subvolume"
      createDirectoryIfMissing True plainDir
      isBtrfsSubvolume plainDir `shouldReturn` False

    it "deleteSubvolumeForcible is exercised end-to-end by the snapshot lifecycle tests above" $ do
      pendingWith "Covered indirectly by 'handles snapshots if supported' and 'deleteAllSnapshots ...' whenever BTRFS is available; not safely unit-testable without a real subvolume."

    it "opens the snapshot's drive_c in the file manager" $ do
      pendingWith "openSnapshotFileManager launches an external GUI app (xdg-open) and isn't unit-testable in CI."
