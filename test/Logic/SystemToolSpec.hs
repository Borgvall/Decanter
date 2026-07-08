module Logic.SystemToolSpec (spec) where

import Test.Hspec
import Logic.SystemTool

spec :: Spec
spec = describe "Logic.SystemTool" $ do

  describe "runSystemTool" $ do
    it "launches an external process without throwing" $ do
      runSystemTool "true" [] `shouldReturn` ()
