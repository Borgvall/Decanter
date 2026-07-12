{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ProgramsSpec (spec) where

import Test.Hspec
import Bottle.Logic.Programs
import Bottle.Types
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  , doesFileExist
  , findExecutable
  )
import System.FilePath ((</>))
import Control.Exception (finally)
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "Bottle.Logic.Programs" $ do

    describe "runCmd" $ do
      it "starts the given command asynchronously with the bottle's merged Wine environment" $ do
        let markerPath = "/tmp/decanter-test-runcmd-marker"
        let bottle = Bottle "Test" "/tmp/decanter-test-runcmd-prefix" SystemWine

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

    describe "isWinetricksAvailable" $ do
      it "matches whether 'winetricks' is on PATH" $ do
        expected <- isJust <$> findExecutable "winetricks"
        isWinetricksAvailable `shouldReturn` expected

    describe "findWineStartMenuLnks" $ do
      it "finds .lnk files in the common and per-user Start Menu, including nested folders, ignoring non-.lnk files" $ do
        cwd <- getCurrentDirectory
        let bottleDir = cwd </> "test-env" </> "ProgramsSpecBottle"
        let driveC = bottleDir </> "drive_c"
        let commonStartMenu = driveC </> "ProgramData/Microsoft/Windows/Start Menu"
        let nestedDir = commonStartMenu </> "Games"
        let userStartMenu = driveC </> "users/alice/AppData/Roaming/Microsoft/Windows/Start Menu"

        createDirectoryIfMissing True nestedDir
        createDirectoryIfMissing True userStartMenu

        writeFile (commonStartMenu </> "Notepad.lnk") ""
        writeFile (commonStartMenu </> "readme.txt") ""
        writeFile (nestedDir </> "Solitaire.lnk") ""
        writeFile (userStartMenu </> "AliceApp.lnk") ""

        let bottle = Bottle "ProgramsSpecBottle" bottleDir SystemWine

        found <- findWineStartMenuLnks bottle `finally` removePathForcibly bottleDir

        found `shouldMatchList`
          [ commonStartMenu </> "Notepad.lnk"
          , nestedDir </> "Solitaire.lnk"
          , userStartMenu </> "AliceApp.lnk"
          ]

      it "returns an empty list when the bottle has no Start Menu directories at all" $ do
        cwd <- getCurrentDirectory
        let bottleDir = cwd </> "test-env" </> "EmptyProgramsSpecBottle"
        createDirectoryIfMissing True bottleDir
        let bottle = Bottle "EmptyProgramsSpecBottle" bottleDir SystemWine

        found <- findWineStartMenuLnks bottle `finally` removePathForcibly bottleDir
        found `shouldBe` []

    it "runWineCfg, runRegedit and runUninstaller are thin 'runCmd' wrappers around real Wine GUI tools" $
      pendingWith "Each just calls 'runCmd' (already covered by its test above) with a fixed wine subcommand; launches a real Wine GUI tool, not mockable here."

    it "runExecutable, runFileWithStart and runWindowsLnk are thin 'runCmd' wrappers that launch a real Wine program" $
      pendingWith "Each just calls 'runCmd' (already covered by its test above) with \"wine\" and a file path; launches a real Wine program, not mockable here."

    it "runWinetricks is a thin 'runCmd' wrapper around the real 'winetricks' tool" $
      pendingWith "Just calls 'runCmd' (already covered by its test above) with \"winetricks\"; launches the real external tool, not mockable here."

    it "runFileManager launches an external GUI app (xdg-open) and isn't unit-testable in CI" $
      pendingWith "Delegates to Logic.SystemTool.runSystemTool, which starts a real host file manager process."
