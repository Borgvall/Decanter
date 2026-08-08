{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Process
  ( getMergedWineEnv
  , killBottleProcesses
  , findBottleScopes
  , md5Hex
  , extractAppIcon
  ) where

import Bottle.Types
import Bottle.Logic.Direct3dWrappers (getDirect3DWrapperState, direct3DWrapperOverrideDllNames)
import System.Process.Typed
import System.Environment (getEnvironment)
import System.Directory (canonicalizePath, doesFileExist)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Control.Exception (try, IOException)
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
direct3DOverrideEntry :: BottleG r -> IO (Maybe String)
direct3DOverrideEntry bottle = do
    state <- getDirect3DWrapperState bottle
    pure $ case direct3DWrapperOverrideDllNames state of
        []    -> Nothing
        names -> Just (intercalate "," names ++ "=native")

-- | WINEDLLOVERRIDES entries. Only meaningful for System Wine bottles --
-- Proton neither integrates with the host desktop nor manages DXVK/
-- vkd3d-proton the way System Wine does, so we leave it alone there.
getWineDllOverridesEnv :: BottleG ExistingRunner -> IO [(String, String)]
getWineDllOverridesEnv bottle = case runner bottle of
    Proton _   -> pure []
    SystemWine -> do
        maybeDirect3DEntry <- direct3DOverrideEntry bottle
        let entries = menuBuilderOverrideEntry : maybeToList maybeDirect3DEntry
        pure [("WINEDLLOVERRIDES", intercalate ";" entries)]

-- | PROTONPATH/PRESSURE_VESSEL_SYSTEMD_SCOPE env entries for a Proton
-- runner, empty for System Wine. Shared by 'getWineOverrides' and
-- 'getIconExtractionWineEnv'. Pure -- this is just a pattern match on
-- "r", no actual I/O happens.
--
-- Takes an 'ExistingRunner': there is no environment to build for a runner
-- that isn't installed, and callers have to establish that beforehand
-- anyway (see "Bottle.Logic".launchableRunner).
getProtonEnv :: ExistingRunner -> [(String, String)]
getProtonEnv r = case r of
    -- PRESSURE_VESSEL_SYSTEMD_SCOPE places the game into a systemd --user
    -- scope (see killProtonProcesses); without it, reliably killing Proton
    -- processes isn't possible.
    Proton p   -> [("PROTONPATH", p), ("PRESSURE_VESSEL_SYSTEMD_SCOPE", "1")]
    SystemWine -> []

-- | Wine-specific environment variables that need to be set/overridden.
getWineOverrides :: BottleG ExistingRunner -> IO [(String, String)]
getWineOverrides bottle = do
    wineDllOverridesEnv <- getWineDllOverridesEnv bottle
    pure $
      [ ("WINEPREFIX", bottlePath bottle)
      ] ++ getProtonEnv (runner bottle)
        ++ wineDllOverridesEnv

-- | Merges bottle-specific Wine environment variables with the host
-- environment (the former take precedence over already-set variables of
-- the same name). Shared by 'getMergedWineEnv' and 'getIconExtractionWineEnv'.
mergeWithHostEnv :: [(String, String)] -> IO [(String, String)]
mergeWithHostEnv wineSpecificEnv = do
    let overrideKeys = map fst wineSpecificEnv

    currentEnv <- getEnvironment

    -- The EA app, does not handle environments variables of a certain length.
    -- The result is, it can not start any game. See
    -- https://discourse.nixos.org/t/failing-to-launch-ea-games-on-nixos/61944.
    --
    -- What upsets it is the mere *presence* of a variable of that length,
    -- not one specific variable -- so this has to stay a length filter; a
    -- targeted blocklist of known-bad names would not be equivalent.
    --
    -- PATH is nevertheless exempt, for two reasons. Dropping it leaves the
    -- child unable to find anything it invokes by name: winetricks looks up
    -- "wineserver" and "taskset" that way and aborts with "wineserver not
    -- found!" without them (Wine itself never notices, because nixpkgs'
    -- wrapper resolves its helpers by absolute path -- which is exactly why
    -- that stayed hidden for so long). And keeping it doesn't reintroduce
    -- the EA problem: Wine/Proton replace the Windows-side PATH with
    -- "C:\windows\system32;..." instead of passing the Unix one through, so
    -- a Windows program never sees it as an over-long variable at all --
    -- verified by launching an EA game (Jedi Fallen Order) with this
    -- exemption in place.
    let eaHack = filter (\(k, v) -> k == "PATH" || length v < 1000) currentEnv

    let filteredEnv = filter (\(k, _) -> k `notElem` overrideKeys) eaHack

    return (wineSpecificEnv ++ filteredEnv)

-- | Builds the environment variables for Wine/Proton
getMergedWineEnv :: BottleG ExistingRunner -> IO [(String, String)]
getMergedWineEnv bottle = getWineOverrides bottle >>= mergeWithHostEnv

-- | Wine environment specifically for icon extraction via winemenubuilder.exe
-- (see 'extractAppIcon'). Deliberately its own, parallel function instead of
-- a Bool flag through 'getWineOverrides'/'getMergedWineEnv': those always set
-- WINEDLLOVERRIDES=winemenubuilder.exe= for System Wine bottles (see
-- 'menuBuilderOverrideEntry'), which would block the very call to
-- winemenubuilder.exe needed here. Since icon extraction needs no Direct3D,
-- we drop WINEDLLOVERRIDES entirely here instead of burdening the
-- functions used everywhere else with a special case.
--
-- DISPLAY/WAYLAND_DISPLAY are removed so Wine doesn't pop up the Gecko/Mono
-- installer dialog for a not-yet-initialized bottle (e.g. in tests) --
-- extractAppIcon should always run headless (same reasoning as for wineboot
-- in Bottle.Logic.createBottleLogic).
getIconExtractionWineEnv :: BottleG ExistingRunner -> IO [(String, String)]
getIconExtractionWineEnv Bottle{..} = do
    env <- mergeWithHostEnv $ ("WINEPREFIX", bottlePath) : getProtonEnv runner
    pure $ filter (\(k, _) -> k `notElem` ["DISPLAY", "WAYLAND_DISPLAY"]) env

-- | Extracts a start-menu application's (".lnk") icon as a PNG file, via
-- Wine's own winemenubuilder.exe ("-t" flag, "thumbnail_lnk"): resolves the
-- .lnk link to the actual .exe and writes the found icon resource via WIC,
-- fully headless (no display needed).
--
-- Runs synchronously, unlike 'runCmd': callers need the finished file right
-- afterwards (e.g. to enter it into a .desktop file). If extraction fails,
-- False is returned instead of throwing an exception -- callers treat this
-- as a best-effort step that e.g. shouldn't prevent adding an
-- application-menu entry overall.
extractAppIcon :: BottleG ExistingRunner -> FilePath -> FilePath -> IO Bool
extractAppIcon bottle lnkPath outputPngPath = do
    env <- getIconExtractionWineEnv bottle
    let args = ["winemenubuilder.exe", "-t", lnkPath, outputPngPath]
    let cmd = case runner bottle of
                Proton _   -> "umu-run"
                SystemWine -> "wine"
    result <- try (runProcess (setEnv env (proc cmd args))) :: IO (Either IOException ExitCode)
    case result of
      Right ExitSuccess -> doesFileExist outputPngPath
      _                 -> pure False

-- | Stops all processes in the bottle.
-- This should happen synchronously so that subsequent operations (like deletion) are safe.
-- Unlike the launch paths, this stays on 'RunnerType': deleteBottleLogic
-- has to be able to clean up a bottle whose runner has since disappeared.
-- A missing runner is handled rather than refused -- 'killProtonProcesses'
-- identifies scopes purely by the prefix path, so it still reaches leftover
-- Proton processes, while "wineserver -k" genuinely has nothing to talk to
-- once Wine is gone.
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = case runner bottle of
  Existing SystemWine -> do
    mergedEnv <- getMergedWineEnv (bottle { runner = SystemWine })
    -- We use runProcess_ instead of startProcess to wait until the command has finished.
    runProcess_ $ setEnv mergedEnv $ proc "wineserver" ["-k"]
  Existing (Proton _)       -> killProtonProcesses bottle
  Missing (MissingProton _) -> killProtonProcesses bottle
  Missing MissingSystemWine -> pure ()

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
killProtonProcesses :: BottleG r -> IO ()
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
findBottleScopes :: BottleG r -> IO [String]
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
