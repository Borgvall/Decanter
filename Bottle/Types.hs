{-# LANGUAGE OverloadedStrings #-}

module Bottle.Types where

import Control.Exception (Exception)
import Data.Text (Text)

-- | 'MissingSystemWine'/'MissingProton' are never persisted themselves --
-- 'SystemWine'/'Proton' keep their original shape precisely so that
-- existing "decanter.cfg" files (which store a derived-'Read'/'Show' of
-- this type) keep parsing unchanged. They are reconstructed fresh on every
-- config load instead, once the persisted 'SystemWine'/'Proton' is found to
-- no longer actually be available (see "Bottle.Logic".loadBottleConfig) --
-- availability can change between runs (Proton build removed, Wine
-- uninstalled), so it can never be a trustworthy on-disk value.
data RunnerType
  = SystemWine
  | Proton FilePath
  | MissingSystemWine
  | MissingProton FilePath
  deriving (Show, Eq, Read)

-- | Thrown if code that builds an actual process invocation for a bottle's
-- runner (Wine/Proton environment or command construction) is somehow
-- reached with a 'MissingSystemWine'/'MissingProton' runner. This should
-- never happen: both the GUI ("Gui.BottleView", via "Bottle.Logic
-- .blockReason") and 'decanter start'/'decanter open' (via the same
-- function) check availability first and refuse to proceed. Hitting this
-- indicates a gating bug rather than an expected runtime condition, so it's
-- modeled as an exception instead of an 'Either'/'Maybe' return value.
newtype RunnerMissingError = RunnerMissingError RunnerType

instance Show RunnerMissingError where
  show (RunnerMissingError r) = "Attempted to build a process invocation for a missing runner: " ++ show r

instance Exception RunnerMissingError

data Bottle = Bottle
  { bottleName :: Text
  , bottlePath :: FilePath
  , runner     :: RunnerType
  } deriving (Show, Eq)

data BottleSnapshot = BottleSnapshot
  { snapshotId   :: Int
  , snapshotName :: Text
  , snapshotPath :: FilePath
  } deriving (Show, Eq)
