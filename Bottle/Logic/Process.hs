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
               -- PRESSURE_VESSEL_SYSTEMD_SCOPE steckt das Spiel in einen
               -- systemd --user Scope (siehe killProtonProcesses), ohne den
               -- ist ein zuverlässiges Beenden von Proton-Prozessen nicht
               -- möglich.
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

-- | Beendet alle Prozesse in der Bottle.
-- Dies sollte synchron geschehen, damit nachfolgende Operationen (wie Löschen) sicher sind.
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = case runner bottle of
  SystemWine -> do
    mergedEnv <- getMergedWineEnv bottle
    -- Wir nutzen runProcess_ statt startProcess, um zu warten bis der Befehl fertig ist.
    runProcess_ $ setEnv mergedEnv $ proc "wineserver" ["-k"]
  Proton _ -> killProtonProcesses bottle

-- | Beendet Proton-Prozesse über ihren systemd --user Scope.
--
-- Proton-Programme laufen (über umu-run/pressure-vessel) in einem eigenen,
-- containerisierten Prozessbaum. Weder ein Signal an den umu-run-Prozess noch
-- ein erneuter Aufruf von "umu-run wineboot -k" erreicht die eigentlichen
-- Wine-Prozesse: Beim Beenden des äußeren Containers werden sie einfach auf
-- init (PID 1) reparentet und laufen unbeeindruckt weiter (empirisch
-- verifiziert; das gleiche, bislang ungelöste Problem hat auch der Heroic
-- Games Launcher, siehe
-- https://github.com/Heroic-Games-Launcher/HeroicGamesLauncher/issues/3879).
--
-- Mit PRESSURE_VESSEL_SYSTEMD_SCOPE=1 (siehe getWineOverrides) steckt
-- pressure-vessel das Spiel stattdessen in einen systemd --user Scope
-- namens "app-steam-app<md5>-<pid>.scope", wobei <md5> deterministisch der
-- MD5-Hash des (kanonischen) WINEPREFIX-Pfads ist -- so berechnet umu-run
-- selbst seine synthetische STEAM_COMPAT_APP_ID (siehe
-- umu/umu_run.py: "prefix_md5 = hashlib.md5(str(pfx)...)"). Einen Scope zu
-- killen erreicht zuverlässig auch bereits verwaiste Kindprozesse, weil ein
-- Scope an die Cgroup gebunden ist, nicht an den (fragilen) Prozessbaum.
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
    _ -> pure ()  -- Kein laufender Scope für diese Bottle -- nichts zu tun.

-- | MD5-Hexdigest eines Strings (über "md5sum", um keine zusätzliche
-- Krypto-Bibliotheksabhängigkeit einzuführen).
md5Hex :: String -> IO String
md5Hex input = do
  (out, _err) <- readProcess_ $ setStdin (byteStringInput (LBS8.pack input)) $ proc "md5sum" []
  pure $ takeWhile (/= ' ') (LBS8.unpack out)
