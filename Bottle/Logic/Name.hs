{-# LANGUAGE OverloadedStrings #-}

-- | The naming rules shared by bottles and snapshots, and the only way to
-- get a name past them.
--
-- Its own module rather than part of "Bottle.Logic" because
-- "Bottle.Logic.Snapshots" needs 'ValidName' too, and "Bottle.Logic"
-- already imports Snapshots -- so the rules have to sit below both. That
-- also puts 'reservedNameSuffixes' next to the rule that enforces it,
-- instead of in the module that merely explains why it exists.
module Bottle.Logic.Name
  ( InvalidNameReason(..)
  , ValidName
  , parseName
  , validNameText
  , explainInvalidName
  , restoreTempSuffix
  , reservedNameSuffixes
  ) where

import Logic.Translation (tr)
import Data.List (isSuffixOf)
import qualified Data.Text as T

-- | Why a name was rejected. Deliberately has no "it was fine" case: that
-- outcome is a 'ValidName', not a reason.
data InvalidNameReason
  = EmptyName
  | NameTooLong
  | ContainsSlash
  | ReservedSuffix
  deriving (Show, Eq)

-- | A name that has passed 'parseName'. Abstract on purpose -- the
-- constructor is not exported, so the only way to obtain one is to run the
-- rules, and functions taking it (see "Bottle.Logic".createBottleLogic,
-- "Bottle.Logic.Snapshots".createSnapshotLogic) need no branch for a name
-- they couldn't use.
newtype ValidName = ValidName T.Text
  deriving (Show, Eq)

-- | Checks a name against the rules, yielding either the reason it was
-- rejected or the name itself as a 'ValidName'.
--
-- Named for what it produces rather than what it asks: it hands back a
-- value the caller could not have built otherwise, so it is a parser, not
-- a predicate.
parseName :: T.Text -> Either InvalidNameReason ValidName
parseName name
  | T.null name = Left EmptyName
  | T.length name > 256 = Left NameTooLong
  | T.elem '/' name = Left ContainsSlash
  -- Reserved so a bottle name can never collide with the temporary
  -- directory restoreSnapshotLogic's crash-safe restore uses.
  | any (`isSuffixOf` T.unpack name) reservedNameSuffixes = Left ReservedSuffix
  | otherwise = Right (ValidName name)

-- | The name inside, for building paths and filling in 'Bottle.Types.Bottle'.
validNameText :: ValidName -> T.Text
validNameText (ValidName name) = name

explainInvalidName :: InvalidNameReason -> T.Text
explainInvalidName reason = case reason of
  EmptyName      -> tr "The name cannot be empty."
  NameTooLong    -> tr "The name is too long (max 256 characters)."
  ContainsSlash  -> tr "The name cannot contain a slash ('/')."
  ReservedSuffix -> tr "The name cannot end with \".restoring\" (reserved for snapshot restore)."

-- | Suffix for the temporary directory
-- "Bottle.Logic.Snapshots".restoreSnapshotLogic builds the restored copy
-- in, before it's swapped into place.
restoreTempSuffix :: String
restoreTempSuffix = ".restoring"

-- | Reserved so a bottle can never be named in a way that collides with
-- 'restoreTempSuffix' (see 'parseName').
reservedNameSuffixes :: [String]
reservedNameSuffixes = [restoreTempSuffix]
