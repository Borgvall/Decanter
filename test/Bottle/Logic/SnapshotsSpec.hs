{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.SnapshotsSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Logic.Snapshots
import Bottle.Types
import System.Directory (createDirectoryIfMissing, removePathForcibly, getCurrentDirectory, doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)

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
          length snaps1 `shouldBe` 1
          snapshotName (head snaps1) `shouldBe` "Initial"

          let testFile = bottlePath bottle </> "testfile.txt"
          writeFile testFile "State 2: With File"
          existsAfterWrite <- doesFileExist testFile
          existsAfterWrite `shouldBe` True

          createSnapshotLogic bottle "WithFile"
          snaps2 <- listSnapshots bottle
          length snaps2 `shouldBe` 2

          let snapInitial = head [ s | s <- snaps2, snapshotName s == "Initial" ]
          restoreSnapshotLogic bottle snapInitial

          -- The file must be gone after restoring the snapshot taken before it existed
          existsAfterRestore1 <- doesFileExist testFile
          existsAfterRestore1 `shouldBe` False

          deleteSnapshotLogic snapInitial
          snaps3 <- listSnapshots bottle
          length snaps3 `shouldBe` 1
          snapshotName (head snaps3) `shouldBe` "WithFile"

          let snapWithFile = head [ s | s <- snaps3, snapshotName s == "WithFile" ]
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

    it "isBtrfsSubvolume returns False for a plain (non-BTRFS) directory" $ do
      cwd <- getCurrentDirectory
      let plainDir = cwd </> "test-env" </> "not-a-subvolume"
      createDirectoryIfMissing True plainDir
      isBtrfsSubvolume plainDir `shouldReturn` False

    it "deleteSubvolumeForcible is exercised end-to-end by the snapshot lifecycle tests above" $ do
      pendingWith "Covered indirectly by 'handles snapshots if supported' and 'deleteAllSnapshots ...' whenever BTRFS is available; not safely unit-testable without a real subvolume."

    it "opens the snapshot's drive_c in the file manager" $ do
      pendingWith "openSnapshotFileManager launches an external GUI app (xdg-open) and isn't unit-testable in CI."
