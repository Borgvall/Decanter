{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Process
  ( getMergedWineEnv
  , killBottleProcesses
  , findBottleScopes
  , md5Hex
  ) where

import Bottle.Types
import Bottle.Logic.Direct3dWrappers (getDirect3DWrapperState, direct3DWrapperOverrideDllNames)
import System.Process.Typed
import System.Environment (getEnvironment)
import System.Directory (canonicalizePath)
import System.Exit (ExitCode(..))
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- | The WINEDLLOVERRIDES entry for winemenubuilder.exe needed to stop Wine
-- from creating Linux desktop integration (Start Menu / ".desktop" entries,
-- MIME associations) for programs installed into a bottle. Decanter is
-- meant to be the only entry point into a bottle's programs; leaving this
-- enabled would let users launch them straight from the host's application
-- menu, bypassing Decanter entirely. An empty load order ("=") disables the
-- DLL outright (see the WINEDLLOVERRIDES syntax in "man wine").
menuBuilderOverrideEntry :: String
menuBuilderOverrideEntry = "winemenubuilder.exe="

-- | The WINEDLLOVERRIDES entry needed for Wine to actually load DXVK's/
-- vkd3d-proton's DLLs instead of its own builtin Direct3D implementation for
-- them (see Bottle.Logic.Direct3dWrappers.direct3DWrapperOverrideDllNames
-- for why placing the DLL file alone isn't enough), if any are needed.
direct3DOverrideEntry :: Bottle -> IO (Maybe String)
direct3DOverrideEntry bottle = do
    state <- getDirect3DWrapperState bottle
    pure $ case direct3DWrapperOverrideDllNames state of
        []    -> Nothing
        names -> Just (intercalate "," names ++ "=native")

-- | WINEDLLOVERRIDES entries. Only meaningful for System Wine bottles --
-- Proton neither integrates with the host desktop nor manages DXVK/
-- vkd3d-proton the way System Wine does, so we leave it alone there.
getWineDllOverridesEnv :: Bottle -> IO [(String, String)]
getWineDllOverridesEnv bottle = case runner bottle of
    Proton _   -> pure []
    SystemWine -> do
        maybeDirect3DEntry <- direct3DOverrideEntry bottle
        let entries = menuBuilderOverrideEntry : maybe [] (: []) maybeDirect3DEntry
        pure [("WINEDLLOVERRIDES", intercalate ";" entries)]

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> IO [(String, String)]
getWineOverrides bottle@Bottle{..} = do
    wineDllOverridesEnv <- getWineDllOverridesEnv bottle
    pure $
      [ ("WINEPREFIX", bottlePath)
      , ("WINEARCH", archToString arch)
      ] ++ (case runner of
               -- PRESSURE_VESSEL_SYSTEMD_SCOPE places the game into a
               -- systemd --user scope (see killProtonProcesses); without
               -- it, reliably killing Proton processes isn't possible.
               Proton p -> [("PROTONPATH", p), ("PRESSURE_VESSEL_SYSTEMD_SCOPE", "1")]
               _ -> [])
        ++ wineDllOverridesEnv

-- | Erstellt die Umgebungsvariablen für Wine/Proton
getMergedWineEnv :: Bottle -> IO [(String, String)]
getMergedWineEnv bottle = do
    wineSpecificEnv <- getWineOverrides bottle
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
--
-- A bottle can have more than one live scope at the same time: every
-- umu-run invocation (e.g. the "wineboot -u" done by createBottleLogic,
-- and separately whatever program the user actually launches) gets its
-- own container and thus its own scope, all sharing the same
-- WINEPREFIX-derived name prefix but a different trailing PID. All of
-- them need to be killed, not just the first one found.
killProtonProcesses :: Bottle -> IO ()
killProtonProcesses bottle = do
  scopeNames <- findBottleScopes bottle
  case scopeNames of
    [] -> pure ()  -- No running scope for this bottle -- nothing to do.
    _  -> runProcess_ $ proc "systemctl" (["--user", "kill"] ++ scopeNames)

-- | Names of all currently running systemd --user scopes belonging to this
-- bottle (see 'killProtonProcesses' for why there can be more than one).
-- Exposed so tests can check for a live/dead scope directly instead of
-- guessing at the wineserver binary's on-disk path, which can differ from
-- what actually shows up in a running process' command line (e.g. behind
-- extra layers of symlinks/wrapper scripts).
findBottleScopes :: Bottle -> IO [String]
findBottleScopes bottle = do
  canonicalPrefix <- canonicalizePath (bottlePath bottle)
  prefixHash <- md5Hex canonicalPrefix
  let scopePattern = "app-steam-app" ++ prefixHash ++ "-*.scope"
  (listExitCode, out) <- readProcessStdout $
    proc "systemctl" ["--user", "list-units", "--type=scope", "--no-legend", "--plain", scopePattern]
  case listExitCode of
    ExitSuccess -> pure [ name | line <- lines (LBS8.unpack out), (name : _) <- [words line] ]
    _           -> pure []

-- | MD5 hex digest of a string (via "md5sum", to avoid introducing an
-- extra crypto library dependency).
md5Hex :: String -> IO String
md5Hex input = do
  (out, _err) <- readProcess_ $ setStdin (byteStringInput (LBS8.pack input)) $ proc "md5sum" []
  pure $ takeWhile (/= ' ') (LBS8.unpack out)
