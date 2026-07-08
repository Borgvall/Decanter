{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ProcessSpec (spec) where

import Test.Hspec
import Bottle.Logic.Process
import Bottle.Types

spec :: Spec
spec = describe "Bottle.Logic.Process" $ do

  describe "getMergedWineEnv" $ do
    it "sets WINEPREFIX and WINEARCH according to the bottle" $ do
      let bottle = Bottle "Test" "/tmp/decanter-test-prefix" SystemWine Win64
      env <- getMergedWineEnv bottle
      lookup "WINEPREFIX" env `shouldBe` Just "/tmp/decanter-test-prefix"
      lookup "WINEARCH" env `shouldBe` Just "win64"
      lookup "PROTONPATH" env `shouldBe` Nothing

    it "sets PROTONPATH when using a Proton runner" $ do
      let bottle = Bottle "Test" "/tmp/decanter-test-prefix" (Proton "/opt/GE-Proton") Win64
      env <- getMergedWineEnv bottle
      lookup "PROTONPATH" env `shouldBe` Just "/opt/GE-Proton"

  describe "killBottleProcesses" $ do
    it "stops any running Wine/Proton processes for the bottle" $ do
      pendingWith "Spawns a real wineserver/umu-run process and isn't safely unit-testable in CI; exercised indirectly by Bottle.Logic's and Bottle.Logic.Snapshots's integration tests."
