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
