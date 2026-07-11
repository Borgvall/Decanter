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
import Control.Concurrent (threadDelay)
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

    describe "findBottleByName" $ do
      let bottleA = Bottle "Alpha" "/tmp/alpha" SystemWine Win64
      let bottleB = Bottle "Beta" "/tmp/beta" SystemWine Win32
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

    describe "runCmd" $ do
      it "starts the given command asynchronously with the bottle's merged Wine environment" $ do
        let markerPath = "/tmp/decanter-test-runcmd-marker"
        let bottle = Bottle "Test" "/tmp/decanter-test-runcmd-prefix" SystemWine Win64

        removePathForcibly markerPath
        runCmd bottle "sh" ["-c", "echo -n \"$WINEPREFIX\" > " ++ markerPath]

        -- runCmd starts the process asynchronously (startProcess, not
        -- runProcess), so poll for the marker file instead of reading it
        -- immediately.
        let waitForMarker attempts
              | attempts <= (0 :: Int) = pure Nothing
              | otherwise = do
                  exists <- doesFileExist markerPath
                  if exists
                    then do
                      -- Force full evaluation while the file still exists:
                      -- readFile is lazy, and 'finally' below removes the
                      -- marker as soon as this action returns.
                      s <- readFile markerPath
                      length s `seq` pure (Just s)
                    else threadDelay 50000 >> waitForMarker (attempts - 1)

        contents <- waitForMarker 40 `finally` removePathForcibly markerPath
        contents `shouldBe` Just (bottlePath bottle)

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
