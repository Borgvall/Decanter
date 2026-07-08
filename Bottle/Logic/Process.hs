{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Process
  ( getMergedWineEnv
  , killBottleProcesses
  , md5Hex
  ) where

import Bottle.Types
import System.Process.Typed
import System.Environment (getEnvironment)
import System.Directory (canonicalizePath)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> [(String, String)]
getWineOverrides Bottle{..} =
    [ ("WINEPREFIX", bottlePath)
    , ("WINEARCH", archToString arch)
    ] ++ case runner of
               -- PRESSURE_VESSEL_SYSTEMD_SCOPE places the game into a
               -- systemd --user scope (see killProtonProcesses); without
               -- it, reliably killing Proton processes isn't possible.
               Proton p -> [("PROTONPATH", p), ("PRESSURE_VESSEL_SYSTEMD_SCOPE", "1")]
               _ -> []

-- | Erstellt die Umgebungsvariablen für Wine/Proton
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv bottle = do
    let wineSpecificEnv = getWineOverrides bottle
    let overrideKeys = map fst wineSpecificEnv

    currentEnv <- getEnvironment

    -- The EA app, does not handle environments variables of a certain length.
    -- The result is, it can not start any game. See
    -- https://discourse.nixos.org/t/failing-to-launch-ea-games-on-nixos/61944.
    let eaHack = filter ((<1000) . length . snd) currentEnv

    let filteredEnv = filter (\(k, _) -> k `notElem` overrideKeys) eaHack

    return (wineSpecificEnv ++ filteredEnv)

-- | Stops all processes in the bottle.
-- This should happen synchronously so that subsequent operations (like deletion) are safe.
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = case runner bottle of
  SystemWine -> do
    mergedEnv <- getMergedWineEnv bottle
    -- We use runProcess_ instead of startProcess to wait until the command has finished.
    runProcess_ $ setEnv mergedEnv $ proc "wineserver" ["-k"]
  Proton _ -> killProtonProcesses bottle

-- | Stops Proton processes via their systemd --user scope.
--
-- Proton programs run (via umu-run/pressure-vessel) in their own,
-- containerized process tree. Neither a signal to the umu-run process nor
-- another call to "umu-run wineboot -k" reaches the actual Wine processes:
-- when the outer container exits, they simply get reparented onto init
-- (PID 1) and keep running unaffected (verified empirically; the Heroic
-- Games Launcher hits the same, still-unresolved problem, see
-- https://github.com/Heroic-Games-Launcher/HeroicGamesLauncher/issues/3879).
--
-- With PRESSURE_VESSEL_SYSTEMD_SCOPE=1 (see getWineOverrides), pressure-
-- vessel instead places the game into a systemd --user scope named
-- "app-steam-app<md5>-<pid>.scope", where <md5> is deterministically the
-- MD5 hash of the (canonical) WINEPREFIX path -- this is exactly how
-- umu-run itself derives its synthetic STEAM_COMPAT_APP_ID (see
-- umu/umu_run.py: "prefix_md5 = hashlib.md5(str(pfx)...)"). Killing a
-- scope reliably reaches already-orphaned child processes too, because a
-- scope is bound to the cgroup, not the (fragile) process tree.
killProtonProcesses :: Bottle -> IO ()
killProtonProcesses bottle = do
  canonicalPrefix <- canonicalizePath (bottlePath bottle)
  prefixHash <- md5Hex canonicalPrefix
  let scopePattern = "app-steam-app" ++ prefixHash ++ "-*.scope"
  (listExitCode, out) <- readProcessStdout $
    proc "systemctl" ["--user", "list-units", "--type=scope", "--no-legend", "--plain", scopePattern]
  case (listExitCode, words (LBS8.unpack out)) of
    (ExitSuccess, scopeName : _) ->
      runProcess_ $ proc "systemctl" ["--user", "kill", scopeName]
    _ -> pure ()  -- No running scope for this bottle -- nothing to do.

-- | MD5 hex digest of a string (via "md5sum", to avoid introducing an
-- extra crypto library dependency).
md5Hex :: String -> IO String
md5Hex input = do
  (out, _err) <- readProcess_ $ setStdin (byteStringInput (LBS8.pack input)) $ proc "md5sum" []
  pure $ takeWhile (/= ' ') (LBS8.unpack out)
