{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Config
  ( saveBottleConfig
  , loadBottleConfig
  ) where

import Bottle.Types
import Bottle.Logic.Runner (findProtonPathByName, compatibilityToolName)
import System.Directory (doesFileExist, findExecutable)
import System.FilePath ((</>))
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Path to a bottle's configuration file
getConfigPath :: FilePath -> FilePath
getConfigPath bottleDir = bottleDir </> "decanter.cfg"

-- | On-disk shape for a bottle's runner -- deliberately not 'RunnerType'
-- itself. A Proton build is identified by its *name* (see
-- 'Bottle.Logic.Runner.compatibilityToolName') rather than its path: unlike
-- a path, a name survives the tool moving between the multiple directories
-- "Bottle.Logic.Runner" now searches (a system package update, a Nix
-- rebuild, or Steam's own precedence changing which directory wins). Never
-- itself holds "missing" state -- see 'resolvePersistedRunner'.
data PersistedRunner = PersistedSystemWine | PersistedProtonName T.Text
  deriving (Show, Read)

toPersistedRunner :: RunnerType -> IO PersistedRunner
toPersistedRunner SystemWine        = pure PersistedSystemWine
toPersistedRunner (Proton p)        = PersistedProtonName <$> compatibilityToolName p
toPersistedRunner MissingSystemWine = pure PersistedSystemWine
toPersistedRunner (MissingProton p) = PersistedProtonName <$> compatibilityToolName p

-- | Saves the bottle's configuration (runner)
saveBottleConfig :: Bottle -> IO ()
saveBottleConfig b = do
    persisted <- toPersistedRunner (runner b)
    writeFile (getConfigPath (bottlePath b)) (show persisted)

-- | Stand-in for the 'Arch' field pre-existing config files (written before
-- 32-bit prefix support was removed) still have, so 'loadBottleConfig' can
-- parse them; the actual value is discarded.
data LegacyArch = Win32 | Win64 deriving (Read)

-- | Loads the bottle's configuration. Understands the current
-- 'PersistedRunner' format, the previous format (a bare 'RunnerType',
-- storing a Proton path directly), and the legacy
-- '(RunnerType, LegacyArch)' tuple format from before 32-bit prefix support
-- was removed -- so bottles created with any older Decanter version don't
-- lose their configured runner. Only 'saveBottleConfig' ever writes the
-- current format; existing files stay in whichever older format they're
-- already in until the next save (e.g. a runner change).
loadBottleConfig :: FilePath -> IO (Maybe RunnerType)
loadBottleConfig bottleDir = do
    let path = getConfigPath bottleDir
    exists <- doesFileExist path
    if exists
        then do
            content <- readFile path
            -- Plain 'reads' for safe parsing
            case reads content :: [(PersistedRunner, String)] of
                [(pr, _)] -> Just <$> resolvePersistedRunner pr
                _ -> case reads content of
                    [(r, _)] -> Just <$> resolveRunnerAvailability r
                    _ -> case reads content :: [((RunnerType, LegacyArch), String)] of
                        [((r, _), _)] -> Just <$> resolveRunnerAvailability r
                        _ -> do
                            putStrLn $ "Could not parse: " ++ path
                            return Nothing
        else return Nothing

-- | Whether "wine" is currently on PATH. Shared by 'resolvePersistedRunner'
-- and 'resolveRunnerAvailability' -- both need the exact same check for
-- SystemWine, just for a runner parsed from a different config format.
isSystemWineAvailable :: IO Bool
isSystemWineAvailable = isJust <$> findExecutable "wine"

-- | Resolves a freshly parsed 'PersistedRunner' to a 'RunnerType', looking
-- up a Proton name's current path fresh on every load (see
-- 'Bottle.Logic.Runner.findProtonPathByName') -- downgrading to
-- 'MissingProton' (keeping just the name, for display) if no tool by that
-- name is currently found anywhere.
resolvePersistedRunner :: PersistedRunner -> IO RunnerType
resolvePersistedRunner PersistedSystemWine = do
    available <- isSystemWineAvailable
    pure $ if available then SystemWine else MissingSystemWine
resolvePersistedRunner (PersistedProtonName name) = do
    mPath <- findProtonPathByName name
    pure $ case mPath of
        Just path -> Proton path
        Nothing   -> MissingProton (T.unpack name)

-- | Re-checks a freshly parsed (previous-format) runner's availability by
-- its literal persisted path, downgrading it to
-- 'MissingSystemWine'/'MissingProton' if that exact path can no longer be
-- found. Deliberately never persisted -- always recomputed on load, since
-- availability can change between runs independently of the bottle's own
-- configuration. Only used for the previous, path-based format; the
-- current format resolves by name instead (see 'resolvePersistedRunner').
resolveRunnerAvailability :: RunnerType -> IO RunnerType
resolveRunnerAvailability SystemWine = do
    available <- isSystemWineAvailable
    pure $ if available then SystemWine else MissingSystemWine
resolveRunnerAvailability (Proton p) = do
    available <- doesFileExist (p </> "compatibilitytool.vdf")
    pure $ if available then Proton p else MissingProton p
-- Never actually persisted (see 'saveBottleConfig'), kept only so this
-- function is total.
resolveRunnerAvailability alreadyMissing = pure alreadyMissing
