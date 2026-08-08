module Bottle.Logic.TestSupport (withTestBottle, testName) where

import Bottle.Logic (createBottleLogic, deleteBottleLogic)
import Bottle.Logic.Name (ValidName, parseName)
import Bottle.Types (BottleG, ExistingRunner, widenRunner)
import Control.Exception (finally)
import Data.Text (Text)
import qualified Data.Text as T

-- | Creates a real bottle named "name" for "runner", runs "action" on it,
-- then always deletes it again afterwards -- even if "action" throws or an
-- expectation inside it fails. Shared by the integration specs that create
-- a real bottle (Process, Direct3dWrappers, Snapshots, Logic) so a failing
-- assertion never leaves a stale bottle (or a still-running wineserver)
-- behind for later tests -- previously each spec repeated its own
-- create/finally-delete pairing, some of them without the 'finally' at
-- all, which skipped cleanup on the first failing expectation.
--
-- Takes the name and runner rather than a ready-made bottle because
-- 'createBottleLogic' does: only an installed runner can initialize a
-- prefix. The bottle handed to "action" (and deleted afterwards) is the
-- one 'createBottleLogic' returns.
--
-- Handed over at @BottleG ExistingRunner@, since it was just created with
-- an installed runner: a test body can pass it straight to the launch
-- paths without re-stating which runner that was. Tests that need the
-- wider type (for 'deleteBottleLogic', 'killBottleProcesses' or
-- 'restoreSnapshotLogic', which all still cope with a runner that has
-- since disappeared) apply 'widenRunner' at the call site.
withTestBottle :: Text -> ExistingRunner -> (BottleG ExistingRunner -> IO ()) -> IO ()
withTestBottle name runner action = do
  bottle <- createBottleLogic (testName name) runner
  action bottle `finally` deleteBottleLogic (widenRunner bottle)

-- | A literal name from a spec, run through 'parseName'. Specs pick their
-- own bottle/snapshot names, so a rejection here is a mistake in the test
-- rather than a case worth handling -- hence the loud failure instead of a
-- 'Maybe' every call site would have to unwrap.
testName :: Text -> ValidName
testName name = case parseName name of
  Right valid  -> valid
  Left reason  -> error $ "test used a name the naming rules reject: "
                          ++ T.unpack name ++ " (" ++ show reason ++ ")"
