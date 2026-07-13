{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ConfigSpec (spec) where

import Test.Hspec
import Bottle.Logic.Config
import Bottle.Types
import System.Directory
  ( createDirectoryIfMissing
  , removePathForcibly
  , getCurrentDirectory
  )
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (finally)

-- | A throwaway directory to read/write "decanter.cfg" in, isolated per
-- test. Unlike Bottle.LogicSpec's XDG-redirecting tests,
-- saveBottleConfig/loadBottleConfig take a plain bottle directory directly
-- and don't need XDG_DATA_HOME touched.
withTestDir :: IO () -> IO ()
withTestDir action = do
  cwd <- getCurrentDirectory
  let testDir = cwd </> "config-spec-test-env"
  createDirectoryIfMissing True testDir
  action `finally` removePathForcibly testDir

testBottleDir :: String -> IO FilePath
testBottleDir name = do
  cwd <- getCurrentDirectory
  pure (cwd </> "config-spec-test-env" </> name)

spec :: Spec
spec = do
  describe "Bottle.Logic.Config" $ around_ withTestDir $ do

    it "loadBottleConfig returns Nothing when no config file exists" $ do
      dir <- testBottleDir "NoConfigTest"
      createDirectoryIfMissing True dir
      loadBottleConfig dir `shouldReturn` Nothing

    it "round-trips a SystemWine runner via saveBottleConfig/loadBottleConfig" $ do
      dir <- testBottleDir "SaveLoadSystemWineTest"
      createDirectoryIfMissing True dir
      saveBottleConfig (Bottle "SaveLoadSystemWineTest" dir SystemWine)
      loadBottleConfig dir `shouldReturn` Just SystemWine

    it "saveBottleConfig persists a Proton runner by name, not by path" $ do
      dir <- testBottleDir "PersistNameTest"
      createDirectoryIfMissing True dir
      saveBottleConfig (Bottle "PersistNameTest" dir (Proton "/some/where/CoolProton"))
      content <- readFile (dir </> "decanter.cfg")
      content `shouldBe` "PersistedProtonName \"CoolProton\""

    it "resolves a persisted Proton name to wherever that name is currently found, even if the path moved" $ do
      dir <- testBottleDir "PersistedNameTest"
      cwd <- getCurrentDirectory
      let extraToolsDir = cwd </> "config-spec-test-env" </> "ExtraCompatTools"
      let protonDir = extraToolsDir </> "MyFakeProton"
      createDirectoryIfMissing True dir
      createDirectoryIfMissing True protonDir
      writeFile (protonDir </> "compatibilitytool.vdf") ""
      writeFile (dir </> "decanter.cfg") "PersistedProtonName \"MyFakeProton\""

      setEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS" extraToolsDir
      (loadBottleConfig dir `shouldReturn` Just (Proton protonDir))
        `finally` unsetEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS"

    it "downgrades a persisted Proton name to MissingProton when no tool by that name exists anymore" $ do
      dir <- testBottleDir "PersistedNameGoneTest"
      createDirectoryIfMissing True dir
      writeFile (dir </> "decanter.cfg") "PersistedProtonName \"GhostProton\""
      loadBottleConfig dir `shouldReturn` Just (MissingProton "GhostProton")

    it "still reads the previous, path-based config format, downgrading a no-longer-existing Proton path to MissingProton" $ do
      dir <- testBottleDir "PathFormatMissingTest"
      createDirectoryIfMissing True dir
      writeFile (dir </> "decanter.cfg") "Proton \"/no/longer/here\""
      loadBottleConfig dir `shouldReturn` Just (MissingProton "/no/longer/here")

    it "still reads the legacy (RunnerType, Arch) tuple config format, downgrading a no-longer-existing Proton path to MissingProton" $ do
      dir <- testBottleDir "LegacyConfigTest"
      createDirectoryIfMissing True dir
      writeFile (dir </> "decanter.cfg") "(Proton \"/legacy/path\",Win64)"
      loadBottleConfig dir `shouldReturn` Just (MissingProton "/legacy/path")

    it "keeps a legacy-format Proton runner intact when its path is still a valid compatibility tool" $ do
      dir <- testBottleDir "LegacyConfigStillValidTest"
      cwd <- getCurrentDirectory
      let protonDir = cwd </> "config-spec-test-env" </> "FakeProton"
      createDirectoryIfMissing True dir
      createDirectoryIfMissing True protonDir
      writeFile (protonDir </> "compatibilitytool.vdf") ""
      writeFile (dir </> "decanter.cfg") ("(Proton " ++ show protonDir ++ ",Win64)")
      loadBottleConfig dir `shouldReturn` Just (Proton protonDir)
