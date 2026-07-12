{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ApplicationMenuSpec (spec) where

import Test.Hspec
import Bottle.Logic.ApplicationMenu
import Bottle.Types
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  , getXdgDirectory
  , XdgDirectory(XdgData)
  , getSymbolicLinkTarget
  , doesPathExist
  )
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
  describe "Bottle.Logic.ApplicationMenu" $ around_ withTestEnvironment $ do
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

    it "removeApplicationMenuSymlink removes the ~/.local/share/applications symlink" $ do
      bottle <- makeMenuTestBottle
      addToApplicationMenu bottle "MyApp" bogusLnkPath "Utility"

      appsDir <- getXdgDirectory XdgData "applications"
      let linkPath = appsDir </> "decanter-MenuTestBottle"
      doesPathExist linkPath `shouldReturn` True

      removeApplicationMenuSymlink bottle
      doesPathExist linkPath `shouldReturn` False
