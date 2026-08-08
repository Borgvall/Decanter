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

-- | A bottle, parameterized over what is known about its runner. The
-- parameter is the *only* place a runner is stored, so a bottle and "the
-- runner to use with it" can't drift apart the way a separate
-- @Bottle -> ExistingRunner@ argument pair could.
--
-- Two instantiations are in use:
--
-- * @BottleG RunnerType@ (the 'Bottle' synonym) -- a bottle as it comes off
--   disk, whose runner may or may not still be installed. The bottle list
--   is deliberately mixed, so this is what 'listExistingBottles' yields.
-- * @BottleG ExistingRunner@ -- a bottle whose runner is known to be
--   installed, and which can therefore be handed to the functions that
--   build a process invocation (see "Bottle.Logic.Process".getMergedWineEnv,
--   "Bottle.Logic.Programs".runCmd). "Bottle.Logic".launchableRunner is the
--   one place that narrows the former into the latter.
--
-- Deliberately not given a name like @RunnableBottle@: the parameter says
-- the runner exists, and nothing more. Whether a bottle may actually launch
-- something additionally depends on its Direct3D wrapper being intact,
-- which no type here carries -- see 'Bottle.Logic.launchableRunner'.
data BottleG r = Bottle
  { bottleName :: Text
  , bottlePath :: FilePath
  , runner     :: r
  } deriving (Show, Eq)

-- | A bottle whose runner may since have been uninstalled -- what
-- 'listExistingBottles' hands out and what the GUI's bottle list holds.
type Bottle = BottleG RunnerType

-- | Forgets that a bottle's runner is known to be installed, so it can be
-- handed to something that accepts any bottle -- e.g. 'deleteBottleLogic',
-- which deliberately still copes with a runner that has since disappeared.
-- The other direction has no such function on purpose: narrowing is a claim
-- about the world that has to be checked, which is
-- 'Bottle.Logic.launchableBottle''s job.
widenRunner :: BottleG ExistingRunner -> Bottle
widenRunner bottle = bottle { runner = Existing (runner bottle) }

data BottleSnapshot = BottleSnapshot
  { snapshotId   :: Int
  , snapshotName :: Text
  , snapshotPath :: FilePath
  } deriving (Show, Eq)
