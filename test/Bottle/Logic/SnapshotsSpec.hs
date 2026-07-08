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

-- | Hilfsfunktion: Setzt eine isolierte Testumgebung auf
withTestEnvironment :: IO () -> IO ()
withTestEnvironment action = do
    -- Wir erstellen einen temporären Ordner für die Tests
    cwd <- getCurrentDirectory
    let testDir = cwd </> "test-env"
    let xdgDataHome = testDir </> ".local" </> "share"

    -- Setup
    createDirectoryIfMissing True xdgDataHome

    -- Wir biegen XDG_DATA_HOME um, damit Decanter in unseren Testordner schreibt
    setEnv "XDG_DATA_HOME" xdgDataHome

    -- Führe den Test aus
    action `finally` do
        -- Teardown: Aufräumen
        removePathForcibly testDir
        unsetEnv "XDG_DATA_HOME"

spec :: Spec
spec = do
  describe "Bottle.Logic.Snapshots" $ around_ withTestEnvironment $ do

    it "handles snapshots if supported" $ do
      bottle <- createBottleObject "SnapshotTestBottle" Win64 SystemWine

      -- Erstelle die Bottle (dies führt wineboot aus, falls Wine installiert ist)
      createBottleLogic bottle

      supportsSnaps <- isSnapshotableBottle bottle

      if supportsSnaps
        then do
          -- 1. Erstelle einen Snapshot
          createSnapshotLogic bottle "Initial"
          snaps1 <- listSnapshots bottle
          length snaps1 `shouldBe` 1
          snapshotName (head snaps1) `shouldBe` "Initial"

          -- 2. Lege eine leere Testdatei in der Bottle an
          let testFile = bottlePath bottle </> "testfile.txt"
          writeFile testFile "State 2: With File"
          existsAfterWrite <- doesFileExist testFile
          existsAfterWrite `shouldBe` True

          -- 3. Erstelle einen zweiten Snapshot
          createSnapshotLogic bottle "WithFile"
          snaps2 <- listSnapshots bottle
          length snaps2 `shouldBe` 2

          -- 4. Stelle den ersten Snapshot wieder her
          let snapInitial = head [ s | s <- snaps2, snapshotName s == "Initial" ]
          restoreSnapshotLogic bottle snapInitial

          -- Zustand prüfen: Datei muss weg sein
          existsAfterRestore1 <- doesFileExist testFile
          existsAfterRestore1 `shouldBe` False

          -- 5. Lösche den ersten Snapshot
          deleteSnapshotLogic snapInitial
          snaps3 <- listSnapshots bottle
          length snaps3 `shouldBe` 1
          snapshotName (head snaps3) `shouldBe` "WithFile"

          -- 6. Stelle den zweiten Snapshot wieder her
          let snapWithFile = head [ s | s <- snaps3, snapshotName s == "WithFile" ]
          restoreSnapshotLogic bottle snapWithFile

          -- Zustand prüfen: Datei muss wieder da sein
          existsAfterRestore2 <- doesFileExist testFile
          existsAfterRestore2 `shouldBe` True
          content <- readFile testFile
          content `shouldBe` "State 2: With File"

          -- 7. Lösche die Bottle
          deleteBottleLogic bottle

          -- Überprüfung: Bottle weg?
          remainingBottles <- listExistingBottles
          let ourBottles = filter (\b -> bottleName b == "SnapshotTestBottle") remainingBottles
          ourBottles `shouldBe` []

        else do
          putStrLn "Skipping snapshot integration tests (no BTRFS detected)"
          -- Wenn keine Snapshots unterstützt werden, sollte die Liste zumindest leer und abrufbar sein
          snaps <- listSnapshots bottle
          snaps `shouldBe` []

          -- Cleanup
          deleteBottleLogic bottle

    it "deleteAllSnapshots removes every snapshot and the snapshot directory" $ do
      bottle <- createBottleObject "DeleteAllSnapshotsTestBottle" Win64 SystemWine
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
