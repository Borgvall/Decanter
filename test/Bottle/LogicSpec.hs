{-# LANGUAGE OverloadedStrings #-}

module Bottle.LogicSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Types
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removePathForcibly, getCurrentDirectory)
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
        pendingWith "Skipping 32-bit test: Wine 32-bit is broken."
      {-
        -- Check if system supports 32-bit Wine
        hasWin32 <- checkSystemWine32Support
        if hasWin32 
          then do
            bottle <- createBottleObject "32bitTest" Win32 SystemWine
            createAndDeleteBottle bottle
          else
            pendingWith "Skipping 32-bit test: Wine 32-bit not supported on this system."
      -}

      it "create and delete 64 bit prefix" $ do
        bottle <- createBottleObject "64bitTest" Win64 SystemWine
        createAndDeleteBottle bottle

      it "persists runner configuration (Proton)" $ do
        pendingWith "UMU-Launcher and Proton currently not available in test environment."

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

    describe "BTRFS/process helpers (used by Bottle.Logic.Snapshots)" $ around_ withTestEnvironment $ do

      it "isBtrfsSubvolume returns False for a plain (non-BTRFS) directory" $ do
        cwd <- getCurrentDirectory
        let plainDir = cwd </> "test-env" </> "not-a-subvolume"
        createDirectoryIfMissing True plainDir
        isBtrfsSubvolume plainDir `shouldReturn` False

      it "deleteSubvolumeForcible is exercised end-to-end by the bottle/snapshot lifecycle tests" $ do
        pendingWith "Covered indirectly by 'create and delete 64 bit prefix' and Bottle.Logic.SnapshotsSpec whenever BTRFS is available; not safely unit-testable without a real subvolume."

      it "runSystemTool launches an external tool without crashing" $ do
        pendingWith "runSystemTool spawns a real external process (e.g. xdg-open) and isn't unit-testable in CI."
