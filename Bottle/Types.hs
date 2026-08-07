{-# LANGUAGE OverloadedStrings #-}

module Bottle.Types where

import Data.Text (Text)

-- | A runner that is actually present and can therefore be used to build a
-- process invocation. Functions that start something (see
-- "Bottle.Logic.Process".getProtonEnv, "Bottle.Logic.Programs".runCmd) take
-- this rather than 'RunnerType', so "what if it's missing?" is a question
-- the type system answers instead of a runtime check.
--
-- Also the exact shape of the *previous* on-disk config format, which
-- stored a derived-'Show' of the bare runner and never contained a missing
-- one (see "Bottle.Logic.Config".loadBottleConfig) -- hence the 'Read'
-- instance, which is load-bearing for those older files.
data ExistingRunner
  = SystemWine
  | Proton FilePath
  deriving (Show, Eq, Read)

-- | A runner a bottle is configured for, but that isn't currently installed
-- (Proton build removed, Wine uninstalled). Never persisted: availability
-- can change between runs, so it can never be a trustworthy on-disk value
-- and is recomputed on every config load.
data MissingRunner
  = MissingSystemWine
  | MissingProton FilePath
  deriving (Show, Eq)

-- | A bottle's configured runner, together with whether it is currently
-- available. Match on it directly to get at the 'ExistingRunner' inside, or
-- ask "Bottle.Logic".launchableRunner, which additionally reports *why* a
-- bottle can't run anything.
data RunnerType
  = Existing ExistingRunner
  | Missing MissingRunner
  deriving (Show, Eq)

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
