module Bottle.Logic.TestSupport (withTestBottle) where

import Bottle.Logic (createBottleObject, createBottleLogic, deleteBottleLogic)
import Bottle.Types (Bottle, ExistingRunner)
import Control.Exception (finally)
import Data.Text (Text)

-- | Creates a real bottle named "name" for "runner", runs "action" on it,
-- then always deletes it again afterwards -- even if "action" throws or an
-- expectation inside it fails. Shared by the integration specs that create
-- a real bottle (Process, Direct3dWrappers, Snapshots, Logic) so a failing
-- assertion never leaves a stale bottle (or a still-running wineserver)
-- behind for later tests -- previously each spec repeated its own
-- create/finally-delete pairing, some of them without the 'finally' at
-- all, which skipped cleanup on the first failing expectation.
--
-- Takes the name and runner rather than a ready-made 'Bottle' because
-- 'createBottleLogic' does: only an installed runner can initialize a
-- prefix. The 'Bottle' handed to "action" (and deleted afterwards) comes
-- from 'createBottleObject' on those same two arguments.
withTestBottle :: Text -> ExistingRunner -> (Bottle -> IO ()) -> IO ()
withTestBottle name runner action = do
  bottle <- createBottleObject name runner
  createBottleLogic name runner
  action bottle `finally` deleteBottleLogic bottle
