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

    it "sets PRESSURE_VESSEL_SYSTEMD_SCOPE for a Proton runner, but not for System Wine" $ do
      let protonBottle = Bottle "Test" "/tmp/decanter-test-prefix" (Proton "/opt/GE-Proton") Win64
      protonEnv <- getMergedWineEnv protonBottle
      lookup "PRESSURE_VESSEL_SYSTEMD_SCOPE" protonEnv `shouldBe` Just "1"

      let systemWineBottle = protonBottle { runner = SystemWine }
      systemWineEnv <- getMergedWineEnv systemWineBottle
      lookup "PRESSURE_VESSEL_SYSTEMD_SCOPE" systemWineEnv `shouldBe` Nothing

  describe "md5Hex" $ do
    it "matches known MD5 test vectors" $ do
      md5Hex "" `shouldReturn` "d41d8cd98f00b204e9800998ecf8427e"
      md5Hex "abc" `shouldReturn` "900150983cd24fb0d6963f7d28e17f72"

  describe "killBottleProcesses" $ do
    it "stops any running Wine/Proton processes for the bottle" $ do
      pendingWith "Spawns a real wineserver process (System Wine) or queries/kills a live systemd --user scope (Proton); not safely unit-testable without a running Wine/Proton process, exercised indirectly by Bottle.Logic's and Bottle.Logic.Snapshots's integration tests."
