{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Process
  ( getMergedWineEnv
  , killBottleProcesses
  ) where

import Bottle.Types
import System.Process.Typed
import System.Environment (getEnvironment)

-- | Wine-spezifische Umgebungsvariablen, die gesetzt/überschrieben werden müssen.
getWineOverrides :: Bottle -> [(String, String)]
getWineOverrides Bottle{..} =
    [ ("WINEPREFIX", bottlePath)
    , ("WINEARCH", archToString arch)
    ] ++ case runner of
               Proton p -> [("PROTONPATH", p)]
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

-- | Beendet alle Prozesse in der Bottle (wineserver -k).
-- Dies sollte synchron geschehen, damit nachfolgende Operationen (wie Löschen) sicher sind.
killBottleProcesses :: Bottle -> IO ()
killBottleProcesses bottle = do
  mergedEnv <- getMergedWineEnv bottle

  let (cmd, args) = case runner bottle of
        SystemWine -> ("wineserver", ["-k"])
        Proton _   -> ("umu-run", ["wineboot", "-k"])

  -- Wir nutzen runProcess_ statt startProcess, um zu warten bis der Befehl fertig ist.
  runProcess_ $ setEnv mergedEnv $ proc cmd args
