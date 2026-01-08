{-# LANGUAGE OverloadedStrings #-}

module Data.VdfSpec (spec) where

import Test.Hspec
import Data.Vdf
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "extractDisplayName" $ do
    it "extrahiert den display_name aus einem typischen Proton-VDF" $ do
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
    
    it "kommt mit Kommentaren in der gleichen Zeile zurecht" $ do
      let line = " \"display_name\" \"Proton 9.0\" // Kommentar hier"
      extractDisplayName line `shouldBe` "Proton 9.0"

    it "ignoriert führende/folgende Leerzeichen und Tabs" $ do
      let line = " \t \"display_name\" \t   \"Custom Proton Build\"  "
      extractDisplayName line `shouldBe` "Custom Proton Build"

    it "gibt leeren Text zurück, wenn der Key fehlt" $ do
      extractDisplayName "{ \"other_key\" \"value\" }" `shouldBe` ""
