module Bottle.Logic.TestSupport (withTestBottle) where

import Bottle.Logic (createBottleLogic, deleteBottleLogic)
import Bottle.Types (Bottle)
import Control.Exception (finally)

-- | Runs 'createBottleLogic' on "bottle", then "action", then always
-- 'deleteBottleLogic' again afterwards -- even if "action" throws or an
-- expectation inside it fails. Shared by the integration specs that create
-- a real bottle (Process, Direct3dWrappers, Snapshots, Logic) so a failing
-- assertion never leaves a stale bottle (or a still-running wineserver)
-- behind for later tests -- previously each spec repeated its own
-- create/finally-delete pairing, some of them without the 'finally' at
-- all, which skipped cleanup on the first failing expectation.
withTestBottle :: Bottle -> (Bottle -> IO ()) -> IO ()
withTestBottle bottle action = do
  createBottleLogic bottle
  action bottle `finally` deleteBottleLogic bottle
