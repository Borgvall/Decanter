{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.RunnerSpec (spec) where

import Test.Hspec
import Bottle.Logic.Runner
import Bottle.Types
import System.Directory (findExecutable)
import Data.Maybe (isJust)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Bottle.Logic.Runner" $ do

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
