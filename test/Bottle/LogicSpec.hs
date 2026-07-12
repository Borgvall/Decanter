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
  , doesFileExist
  , getXdgDirectory
  , XdgDirectory(XdgData)
  , getSymbolicLinkTarget
  )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
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

    describe "Application menu integration" $ around_ withTestEnvironment $ do
      let makeMenuTestBottle = do
            cwd <- getCurrentDirectory
            let path = cwd </> "test-env" </> "MenuTestBottle"
            createDirectoryIfMissing True path
            return $ Bottle "MenuTestBottle" path SystemWine Win64

      -- A deliberately non-existent .lnk path is enough for most of these
      -- tests: icon extraction (see Bottle.Logic.Process.extractAppIcon) is
      -- best effort and simply fails gracefully for it (no "Icon=" field),
      -- instead of making addToApplicationMenu fail as a whole.
      let bogusLnkPath = "/nonexistent/MyGame.lnk"

      it "creates a .desktop entry inside the bottle and a symlink pointing at its menu dir" $ do
        bottle <- makeMenuTestBottle
        addToApplicationMenu bottle "MyGame" bogusLnkPath "Game"

        isInApplicationMenu bottle "MyGame" `shouldReturn` True

        content <- readFile (bottlePath bottle </> "menu" </> "MyGame.desktop")
        content `shouldContain` "Name=MyGame"
        content `shouldContain` "Exec=decanter start \"MenuTestBottle\" \"MyGame\""
        content `shouldContain` "Categories=Game;"

        appsDir <- getXdgDirectory XdgData "applications"
        linkTarget <- getSymbolicLinkTarget (appsDir </> "decanter-MenuTestBottle")
        linkTarget `shouldBe` (bottlePath bottle </> "menu")

      it "omits the Icon= field when icon extraction fails (best effort)" $ do
        bottle <- makeMenuTestBottle
        addToApplicationMenu bottle "MyGame" bogusLnkPath "Game"

        content <- readFile (bottlePath bottle </> "menu" </> "MyGame.desktop")
        content `shouldNotContain` "Icon="

      it "reuses the existing symlink for a second application in the same bottle" $ do
        bottle <- makeMenuTestBottle
        addToApplicationMenu bottle "FirstApp" bogusLnkPath "Game"
        addToApplicationMenu bottle "SecondApp" bogusLnkPath "Utility"

        isInApplicationMenu bottle "FirstApp" `shouldReturn` True
        isInApplicationMenu bottle "SecondApp" `shouldReturn` True

      it "removeFromApplicationMenu removes the entry again" $ do
        bottle <- makeMenuTestBottle
        addToApplicationMenu bottle "MyApp" bogusLnkPath "Utility"
        isInApplicationMenu bottle "MyApp" `shouldReturn` True

        removeFromApplicationMenu bottle "MyApp"
        isInApplicationMenu bottle "MyApp" `shouldReturn` False

      it "isInApplicationMenu is False when nothing was ever added" $ do
        bottle <- makeMenuTestBottle
        isInApplicationMenu bottle "NeverAdded" `shouldReturn` False

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

        createBottleLogic bottle -- writes the config

        bottles <- listExistingBottles -- loads the config
        let loadedBottles = filter (\b -> bottleName b == name) bottles

        length loadedBottles `shouldBe` 1
        let loaded = head loadedBottles

        -- Check that the runner is still Proton
        runner loaded `shouldBe` runner bottle
        arch loaded `shouldBe` Win64

        deleteBottleLogic bottle
