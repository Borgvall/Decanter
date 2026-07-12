{-# LANGUAGE OverloadedStrings #-}

module Data.VdfSpec (spec) where

import Test.Hspec
import Data.Vdf
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "extractDisplayName" $ do
    it "extracts the display_name from a typical Proton VDF" $ do
      let vdfContent = T.unlines 
            [ "\"compatibilitytools\""
            , "{"
            , "  \"compat_tools\""
            , "  {"
            , "    \"GE-Proton10-25\" // Internal name"
            , "    {"
            , "      \"install_path\" \".\""
            , "      \"display_name\" \"GE-Proton10-25\""
            , "      \"from_oslist\"  \"windows\""
            , "      \"to_oslist\"    \"linux\""
            , "    }"
            , "  }"
            , "}"
            ]
      extractDisplayName vdfContent `shouldBe` "GE-Proton10-25"
    
    it "handles comments on the same line" $ do
      let line = " \"display_name\" \"Proton 9.0\" // comment here"
      extractDisplayName line `shouldBe` "Proton 9.0"

    it "ignores leading/trailing spaces and tabs" $ do
      let line = " \t \"display_name\" \t   \"Custom Proton Build\"  "
      extractDisplayName line `shouldBe` "Custom Proton Build"

    it "returns empty text when the key is missing" $ do
      extractDisplayName "{ \"other_key\" \"value\" }" `shouldBe` ""
