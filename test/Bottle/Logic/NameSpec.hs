{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.NameSpec (spec) where

import Test.Hspec
import Bottle.Logic.Name
import Data.Either (isRight)
import qualified Data.Text as T

-- | The reason a name was rejected, or Nothing if it was accepted -- lets
-- the rejection cases below name the expected reason instead of only
-- asserting "not accepted", which the previous 'shouldNotBe Valid' shape
-- couldn't distinguish.
rejection :: T.Text -> Maybe InvalidNameReason
rejection = either Just (const Nothing) . parseName

spec :: Spec
spec = do
  describe "Bottle.Logic.Name" $ do

    describe "parseName" $ do
      it "accepts valid names" $ do
        parseName "MyBottle" `shouldSatisfy` isRight
        parseName "Gaming-Setup_2024" `shouldSatisfy` isRight

      it "rejects empty names" $
        rejection "" `shouldBe` Just EmptyName

      it "rejects names containing slashes" $ do
        rejection "Hack/Me" `shouldBe` Just ContainsSlash
        rejection "/RootBottle" `shouldBe` Just ContainsSlash

      it "rejects overly long names" $ do
        let longName = T.pack $ replicate 300 'a'
        rejection longName `shouldBe` Just NameTooLong

      it "rejects names ending in a reserved restore-marker suffix" $
        rejection "MyBottle.restoring" `shouldBe` Just ReservedSuffix

      -- The point of the newtype: what comes back out is what went in, so
      -- callers can build paths from it without re-deriving anything.
      it "hands back the name it was given" $
        fmap validNameText (parseName "MyBottle") `shouldBe` Right "MyBottle"

    -- Ties the constant to the rule that consumes it: adding a suffix to
    -- the list without 'parseName' honouring it would fail here, rather
    -- than only showing up as a bottle colliding with a half-finished
    -- snapshot restore.
    describe "reservedNameSuffixes" $ do
      it "makes parseName reject a name ending in any of them" $
        mapM_ (\suffix -> rejection (T.pack ("MyBottle" ++ suffix))
                            `shouldBe` Just ReservedSuffix)
              reservedNameSuffixes

      it "covers the suffix restoreSnapshotLogic actually uses" $
        restoreTempSuffix `shouldSatisfy` (`elem` reservedNameSuffixes)

    describe "explainInvalidName" $ do
      it "explains every rejection reason non-emptily" $
        mapM_ (\reason -> explainInvalidName reason `shouldNotBe` "")
              [EmptyName, NameTooLong, ContainsSlash, ReservedSuffix]

      it "names the reserved suffix it is about" $
        explainInvalidName ReservedSuffix `shouldSatisfy`
          T.isInfixOf (T.pack restoreTempSuffix)
