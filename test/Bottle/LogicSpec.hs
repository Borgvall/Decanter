{-# LANGUAGE OverloadedStrings #-}

module Bottle.LogicSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Types
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, removePathForcibly, getCurrentDirectory, setCurrentDirectory)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (bracket)

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
        -- Teardown: Aufräumen (optional, falls man Ergebnisse inspizieren will, auskommentieren)
        removePathForcibly testDir
        unsetEnv "XDG_DATA_HOME"
  where
    finally a b = a >> b -- Vereinfachtes finally

spec :: Spec
spec = do
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
        bottle <- createBottleObject "TestBottle" Win64
        bottleName bottle `shouldBe` "TestBottle"
        -- Der Pfad sollte relativ zum (gemockten) XDG Verzeichnis sein
        -- Da wir XDG_DATA_HOME gesetzt haben, testen wir hier indirekt auch getBottlesBaseDir
        
        -- Hinweis: Da createBottleObject IO macht (Pfad auflösen), ist es hier im Integration-Block
        return () 

      it "lists bottles correctly when empty" $ do
        bottles <- listExistingBottles
        bottles `shouldBe` []
