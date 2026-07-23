{-# LANGUAGE OverloadedStrings #-}

module Bottle.LogicSpec (spec) where

import Test.Hspec
import Bottle.Logic
import Bottle.Types
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

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
  -- Fix for CI/CD environments, which often default to ASCII (C/POSIX)
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

      it "rejects names ending in a reserved restore-marker suffix" $ do
        checkNameValidity "MyBottle.restoring" `shouldNotBe` Valid

    describe "isEngineFamilyChange" $ do
      it "is True when switching from System Wine to Proton" $ do
        isEngineFamilyChange SystemWine (Proton "/path/to/proton-a") `shouldBe` True

      it "is True when switching from Proton to System Wine" $ do
        isEngineFamilyChange (Proton "/path/to/proton-a") SystemWine `shouldBe` True

      it "is True across a missing runner too, as long as the family differs" $ do
        isEngineFamilyChange MissingSystemWine (Proton "/path/to/proton-a") `shouldBe` True
        isEngineFamilyChange (MissingProton "/path/to/proton-a") SystemWine `shouldBe` True

      it "is False when switching between two different Proton builds" $ do
        isEngineFamilyChange (Proton "/path/to/proton-a") (Proton "/path/to/proton-b") `shouldBe` False

      it "is False when the runner doesn't actually change" $ do
        isEngineFamilyChange SystemWine SystemWine `shouldBe` False
        isEngineFamilyChange (Proton "/path/to/proton-a") (Proton "/path/to/proton-a") `shouldBe` False

    describe "findBottleByName" $ do
      let bottleA = Bottle "Alpha" "/tmp/alpha" SystemWine
      let bottleB = Bottle "Beta" "/tmp/beta" SystemWine
      let bottles = [bottleA, bottleB]

      it "finds a bottle by its exact name" $ do
        findBottleByName "Alpha" bottles `shouldBe` Just bottleA
        findBottleByName "Beta" bottles `shouldBe` Just bottleB

      it "is case-sensitive" $ do
        findBottleByName "alpha" bottles `shouldBe` Nothing

      it "returns Nothing for an unknown name" $ do
        findBottleByName "Gamma" bottles `shouldBe` Nothing

    describe "findAppLnkByName" $ do
      let lnkPaths =
            [ "/prefix/drive_c/ProgramData/Start Menu/Notepad.lnk"
            , "/prefix/drive_c/users/alice/Start Menu/Notepad.lnk"
            , "/prefix/drive_c/ProgramData/Start Menu/Solitaire.lnk"
            ]

      it "finds an app by its display name (basename without extension)" $ do
        findAppLnkByName "Solitaire" lnkPaths `shouldBe` Just "/prefix/drive_c/ProgramData/Start Menu/Solitaire.lnk"

      it "returns the first match if the name is ambiguous" $ do
        findAppLnkByName "Notepad" lnkPaths `shouldBe` Just "/prefix/drive_c/ProgramData/Start Menu/Notepad.lnk"

      it "is case-sensitive" $ do
        findAppLnkByName "notepad" lnkPaths `shouldBe` Nothing

      it "returns Nothing for an unknown name" $ do
        findAppLnkByName "Unknown" lnkPaths `shouldBe` Nothing

    describe "Bottle Management (Integration)" $ around_ withTestEnvironment $ do


      it "creates a bottle object with correct paths" $ do
        bottle <- createBottleObject "TestBottle" SystemWine
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

      it "create and delete a bottle" $ do
        bottle <- createBottleObject "CreateDeleteTest" SystemWine
        createAndDeleteBottle bottle

      it "persists runner configuration (Proton)" $ do
        pendingWith "UMU-Launcher and Proton currently not available in test environment."

        let name = "ProtonConfigTest"
        bottle <- createBottleObject name (Proton "/Test/Path")

        createBottleLogic bottle -- writes the config

        bottles <- listExistingBottles -- loads the config
        let loadedBottles = filter (\b -> bottleName b == name) bottles

        -- Check that the runner is still Proton
        case loadedBottles of
          [loaded] -> runner loaded `shouldBe` runner bottle
          _ -> expectationFailure $ "Expecting exactly one bottle, got: " ++ show loadedBottles

        deleteBottleLogic bottle

      -- Config-file-format-specific coverage (legacy formats, persisted-
      -- Proton-by-name resolution, ...) lives in Bottle.Logic.ConfigSpec
      -- now, testing Bottle.Logic.Config's saveBottleConfig/loadBottleConfig
      -- directly rather than through listExistingBottles.

      describe "blockReason / explainBlockReason" $ do
        let dummyBottle = Bottle "BlockReasonTest" "/nonexistent"

        it "reports RunnerMissing for MissingSystemWine, without touching the filesystem" $ do
          blockReason (dummyBottle MissingSystemWine) `shouldReturn` Just (RunnerMissing MissingSystemWine)

        it "reports RunnerMissing for MissingProton, without touching the filesystem" $ do
          blockReason (dummyBottle (MissingProton "/legacy/path")) `shouldReturn` Just (RunnerMissing (MissingProton "/legacy/path"))

        it "explains a missing System Wine runner" $ do
          explainBlockReason (RunnerMissing MissingSystemWine) `shouldNotBe` ""

        it "explains a missing Proton runner, naming it" $ do
          let explanation = explainBlockReason (RunnerMissing (MissingProton "/some/path/GE-Proton10-25"))
          T.isInfixOf "GE-Proton10-25" explanation `shouldBe` True

        it "explains a dangling Direct3D wrapper" $ do
          explainBlockReason Direct3DWrapperDangling `shouldNotBe` ""
