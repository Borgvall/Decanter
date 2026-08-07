{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.RunnerSpec (spec) where

import Test.Hspec
import Bottle.Logic.Runner
import Bottle.Types
import System.Directory (findExecutable, createDirectoryIfMissing, getCurrentDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Sets STEAM_EXTRA_COMPAT_TOOLS_PATHS to a throwaway directory containing
-- a single fake compatibility tool ("MyFakeProton"), so 'findProtonPathByName'
-- can be tested against a real, controlled filesystem scan instead of
-- whatever Proton builds happen to be installed on the machine running the
-- tests (the two system-wide directories 'compatibilityToolSearchDirs'
-- also searches aren't test-injectable, but the env var already is).
withExtraCompatToolsDir :: IO () -> IO ()
withExtraCompatToolsDir action = do
  cwd <- getCurrentDirectory
  let testDir = cwd </> "runner-spec-test-env"
  let toolDir = testDir </> "MyFakeProton"
  createDirectoryIfMissing True toolDir
  writeFile (toolDir </> "compatibilitytool.vdf") ""
  setEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS" testDir
  action `finally` do
    removePathForcibly testDir
    unsetEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS"

spec :: Spec
spec = do
  describe "Bottle.Logic.Runner" $ do

    describe "engineFamily" $ do
      it "puts System Wine and Proton runners in their respective family" $ do
        engineFamily SystemWine          `shouldBe` WineEngine
        engineFamily (Proton "/opt/ge")  `shouldBe` ProtonEngine

      -- A missing runner keeps its family: it's still a bottle of that
      -- engine, just one whose runner is currently unavailable. Callers that
      -- care about availability use 'Bottle.Logic.blockReason' instead.
      it "keeps the family of a runner that is currently missing" $ do
        engineFamily MissingSystemWine        `shouldBe` WineEngine
        engineFamily (MissingProton "/opt/ge") `shouldBe` ProtonEngine

      it "reports a change between the two families, but not between Proton builds" $ do
        let differs a b = engineFamily a /= engineFamily b
        differs SystemWine (Proton "/opt/ge") `shouldBe` True
        differs (Proton "/opt/ge") (Proton "/opt/umu") `shouldBe` False
        differs SystemWine MissingSystemWine `shouldBe` False

    describe "getRunnerTypeDisplayName" $ do
      it "falls back to \"Proton (<dirname>)\" when compatibilitytool.vdf is missing" $ do
        getRunnerTypeDisplayName (Proton "/nonexistent/GE-Proton10-25")
          `shouldReturn` "Proton (GE-Proton10-25)"

      it "returns a non-empty name for System Wine" $ do
        name <- getRunnerTypeDisplayName SystemWine
        T.null name `shouldBe` False

    describe "getAvailableRunners" $ do
      it "includes SystemWine exactly when 'wine' is on PATH" $ do
        wineOnPath <- isJust <$> findExecutable "wine"
        runners <- getAvailableRunners
        (SystemWine `elem` runners) `shouldBe` wineOnPath

    describe "compatibilityToolSearchDirs" $ do
      it "searches the system-wide and user directories, in Steam's own precedence order, when no extra paths are set" $ do
        compatibilityToolSearchDirs "/home/user" Nothing `shouldBe`
          [ "/usr/share/steam/compatibilitytools.d"
          , "/usr/local/share/steam/compatibilitytools.d"
          , "/home/user/.steam/root/compatibilitytools.d"
          ]

      it "inserts colon-separated STEAM_EXTRA_COMPAT_TOOLS_PATHS entries between the system and user directories" $ do
        compatibilityToolSearchDirs "/home/user" (Just "/opt/toolsA:/opt/toolsB") `shouldBe`
          [ "/usr/share/steam/compatibilitytools.d"
          , "/usr/local/share/steam/compatibilitytools.d"
          , "/opt/toolsA"
          , "/opt/toolsB"
          , "/home/user/.steam/root/compatibilitytools.d"
          ]

      it "ignores an empty or unset STEAM_EXTRA_COMPAT_TOOLS_PATHS" $ do
        compatibilityToolSearchDirs "/home/user" (Just "") `shouldBe`
          compatibilityToolSearchDirs "/home/user" Nothing

    describe "dedupToolsByName" $ do
      it "keeps every tool when all names are distinct" $ do
        dedupToolsByName [("GE-Proton10-25", "/usr/share/.../GE-Proton10-25"), ("proton-tkg", "/opt/.../proton-tkg")]
          `shouldBe` [("GE-Proton10-25", "/usr/share/.../GE-Proton10-25"), ("proton-tkg", "/opt/.../proton-tkg")]

      it "lets a later (higher-precedence) entry override an earlier one with the same name" $ do
        dedupToolsByName
          [ ("GE-Proton10-25", "/usr/share/steam/compatibilitytools.d/GE-Proton10-25")
          , ("proton-tkg", "/opt/toolsA/proton-tkg")
          , ("GE-Proton10-25", "/home/user/.steam/root/compatibilitytools.d/GE-Proton10-25")
          ]
          `shouldBe`
          [ ("proton-tkg", "/opt/toolsA/proton-tkg")
          , ("GE-Proton10-25", "/home/user/.steam/root/compatibilitytools.d/GE-Proton10-25")
          ]

    describe "findProtonPathByName" $ around_ withExtraCompatToolsDir $ do
      it "finds a tool placed under STEAM_EXTRA_COMPAT_TOOLS_PATHS by its directory name" $ do
        cwd <- getCurrentDirectory
        let toolDir = cwd </> "runner-spec-test-env" </> "MyFakeProton"
        findProtonPathByName "MyFakeProton" `shouldReturn` Just toolDir

      it "returns Nothing for a name no currently available tool has" $ do
        findProtonPathByName "NoSuchProtonBuild" `shouldReturn` Nothing

    describe "compatibilityToolName" $ do
      it "prefers the VDF's display_name over the directory's own basename" $ do
        cwd <- getCurrentDirectory
        let toolDir = cwd </> "runner-spec-test-env" </> "SomeOpaqueDirName"
        createDirectoryIfMissing True toolDir
        writeFile (toolDir </> "compatibilitytool.vdf") $ unlines
          [ "\"compatibilitytools\""
          , "{"
          , "  \"compat_tools\""
          , "  {"
          , "    \"proton_experimental\""
          , "    {"
          , "      \"display_name\" \"GE-Proton10-25\""
          , "    }"
          , "  }"
          , "}"
          ]
        (compatibilityToolName toolDir `shouldReturn` "GE-Proton10-25")
          `finally` removePathForcibly (cwd </> "runner-spec-test-env")

      it "falls back to the directory's basename when compatibilitytool.vdf has no display_name" $ do
        cwd <- getCurrentDirectory
        let toolDir = cwd </> "runner-spec-test-env" </> "GE-Proton10-25"
        createDirectoryIfMissing True toolDir
        writeFile (toolDir </> "compatibilitytool.vdf") ""
        (compatibilityToolName toolDir `shouldReturn` "GE-Proton10-25")
          `finally` removePathForcibly (cwd </> "runner-spec-test-env")

      it "falls back to the directory's basename when there is no compatibilitytool.vdf at all" $ do
        compatibilityToolName "/nonexistent/GE-Proton10-25" `shouldReturn` "GE-Proton10-25"
