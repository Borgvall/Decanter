{-# LANGUAGE OverloadedStrings #-}

module Bottle.LogicSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Types
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removePathForcibly, getCurrentDirectory, doesFileExist)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

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
  -- Fix für CI/CD Umgebungen, die oft auf ASCII (C/POSIX) stehen
  runIO $ setLocaleEncoding utf8
  
  describe "Bottle.Logic" $ do
    
    describe "checkNameValidity" $ do
      it "accepts valid names" $ do
        checkNameValidity "MyBottle" `shouldBe` Valid
        checkNameValidity "Gaming-Setup_2024" `shouldBe` Valid

      it "rejects empty names" $ do
        checkNameValidity "" `shouldNotBe` Valid

      it "rejects names containing slashes" $ do
        checkNameValidity "Hack/Me" `shouldNotBe` Valid
        checkNameValidity "/RootBottle" `shouldNotBe` Valid

      it "rejects overly long names" $ do
        let longName = T.pack $ replicate 300 'a'
        checkNameValidity longName `shouldNotBe` Valid

    describe "Architecture handling" $ do
      it "converts Arch correctly to string" $ do
        archToString Win32 `shouldBe` "win32"
        archToString Win64 `shouldBe` "win64"

    -- Integrationstests mit Dateisystem
    describe "Bottle Management (Integration)" $ around_ withTestEnvironment $ do
      
      it "creates a bottle object with correct paths" $ do
        bottle <- createBottleObject "TestBottle" Win64 SystemWine
        bottleName bottle `shouldBe` "TestBottle"
        return () 

      it "lists bottles correctly when empty" $ do
        bottles <- listExistingBottles
        bottles `shouldBe` []

      let createAndDeleteBottle bottle = do
            createBottleLogic bottle
            bottles <- listExistingBottles
            case bottles of
              [listedBottle] -> listedBottle `shouldBe` bottle
              _ -> expectationFailure $ "Expecting exactly one bottle, got :" ++ show bottles
            deleteBottleLogic bottle
            noBottles <- listExistingBottles
            noBottles `shouldBe` []

      it "create and delete 32 bit prefix" $ do
        -- Check if system supports 32-bit Wine
        hasWin32 <- checkSystemWine32Support
        if hasWin32 
          then do
            bottle <- createBottleObject "32bitTest" Win32 SystemWine
            createAndDeleteBottle bottle
          else pendingWith "Skipping 32-bit test: Wine 32-bit not supported on this system."

      it "create and delete 64 bit prefix" $ do
        bottle <- createBottleObject "64bitTest" Win64 SystemWine
        createAndDeleteBottle bottle

      it "persists runner configuration (Proton)" $ do
        let name = "ProtonConfigTest"
        bottle <- createBottleObject name Win64 (Proton "/Test/Path")
        
        -- Erstellen (schreibt Config)
        createBottleLogic bottle
        
        -- Listen (lädt Config)
        bottles <- listExistingBottles
        let loadedBottles = filter (\b -> bottleName b == name) bottles
        
        length loadedBottles `shouldBe` 1
        let loaded = head loadedBottles
        
        -- Prüfung: Ist der Runner immer noch Proton?
        runner loaded `shouldBe` runner bottle
        arch loaded `shouldBe` Win64
        
        -- Cleanup
        deleteBottleLogic bottle

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
