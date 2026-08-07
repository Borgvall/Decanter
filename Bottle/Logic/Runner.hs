{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Runner
  ( getAvailableRunners
  , getRunnerTypeDisplayName
  , compatibilityToolSearchDirs
  , dedupToolsByName
  , findProtonPathByName
  , compatibilityToolName
  , EngineFamily(..)
  , engineFamily
  ) where

import Bottle.Types
import Data.Vdf (extractDisplayName)
import Logic.Translation (tr)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    , findExecutable
    , getHomeDirectory
    )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeBaseName)
import System.Process.Typed
import Control.Monad (filterM)
import Data.List (nubBy)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS8

-- | Which of the two engines behind a 'RunnerType' actually sets up and
-- maintains a prefix. Several decisions depend on this rather than on the
-- concrete runner: whether Decanter manages the Direct3D wrapper itself
-- (Proton brings its own), whether winetricks may be offered (it drives the
-- host's Wine), and whether a runner change crosses the engine boundary.
data EngineFamily
  = WineEngine
  | ProtonEngine
  deriving (Show, Eq)

-- | A missing runner keeps its family: 'MissingSystemWine' is still a
-- System Wine bottle, just one whose Wine is currently gone. Callers that
-- care about availability ask "Bottle.Logic".launchableRunner instead.
--
-- Deliberately a data type rather than an @isProton@/@isSystemWine@ pair of
-- predicates: this way a new 'RunnerType' constructor fails to compile here
-- (in exactly one place), and a third engine fails to compile at every
-- 'case' on 'EngineFamily'. A 'Bool' would silently absorb both.
engineFamily :: RunnerType -> EngineFamily
engineFamily (Existing SystemWine)       = WineEngine
engineFamily (Missing MissingSystemWine) = WineEngine
engineFamily (Existing (Proton _))       = ProtonEngine
engineFamily (Missing (MissingProton _)) = ProtonEngine

-- | Directories to scan for compatibility tools (Proton builds), in the
-- same low-to-high precedence order Steam itself uses -- a tool discovered
-- in a later directory overrides one of the same name found in an earlier
-- one (see 'dedupToolsByName'). This mirrors Steam's own search, added in
-- the 2019-10-30 Steam client update per
-- https://github.com/ValveSoftware/steam-for-linux/issues/6310#issuecomment-511468221 :
--
--   /usr/share/steam/compatibilitytools.d
--   /usr/local/share/steam/compatibilitytools.d
--   $STEAM_EXTRA_COMPAT_TOOLS_PATHS (colon-separated)
--   ~/.steam/root/compatibilitytools.d
--
-- The system-wide paths matter for Decanter specifically: they let a
-- Proton build be provided by the system's own package manager (or a Nix
-- profile placing files there) instead of requiring a per-user Steam
-- install.
compatibilityToolSearchDirs :: FilePath -> Maybe String -> [FilePath]
compatibilityToolSearchDirs home extraPathsEnv =
  [ "/usr/share/steam/compatibilitytools.d"
  , "/usr/local/share/steam/compatibilitytools.d"
  ] ++ extraPaths ++
  [ home </> ".steam/root/compatibilitytools.d" ]
  where
    extraPaths = filter (not . null) (splitOnColon (fromMaybe "" extraPathsEnv))
    splitOnColon s = case break (== ':') s of
      (chunk, [])      -> [chunk]
      (chunk, _ : rest) -> chunk : splitOnColon rest

-- | Keeps only the highest-precedence entry for each compatibility tool
-- name, given a list of (name, path) pairs in low-to-high precedence order
-- (as produced by scanning 'compatibilityToolSearchDirs' in order). Mirrors
-- Steam's own "last-registered wins" behaviour for same-named tools.
dedupToolsByName :: [(String, FilePath)] -> [(String, FilePath)]
dedupToolsByName = reverse . nubBy (\a b -> fst a == fst b) . reverse

-- | Compatibility tools (name, path) found directly inside one directory:
-- subdirectories that contain a "compatibilitytool.vdf". The name is each
-- tool's 'compatibilityToolName', not the directory entry itself.
findCompatibilityToolsIn :: FilePath -> IO [(String, FilePath)]
findCompatibilityToolsIn dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      toolDirs <- filterM (\e -> do
          let fullPath = dir </> e
          isDir <- doesDirectoryExist fullPath
          hasVdf <- doesFileExist (fullPath </> "compatibilitytool.vdf")
          return (isDir && hasVdf)
          ) entries
      mapM (\e -> do
          let fullPath = dir </> e
          name <- compatibilityToolName fullPath
          pure (T.unpack name, fullPath)
          ) toolDirs

-- | The "display_name" from a compatibility tool's "compatibilitytool.vdf"
-- at "path", or "" if the file is missing or has no "display_name". Shared
-- by 'compatibilityToolName' and 'getRunnerTypeDisplayName', which each
-- apply their own fallback for the empty case.
readVdfDisplayName :: FilePath -> IO T.Text
readVdfDisplayName path = do
  let vdfPath = path </> "compatibilitytool.vdf"
  exists <- doesFileExist vdfPath
  if exists
    then extractDisplayName . T.pack <$> readFile vdfPath
    else pure ""

-- | The name identifying a compatibility tool at the given path: the
-- "display_name" from its "compatibilitytool.vdf" (the same string already
-- shown in the runner-selection UI, see 'getRunnerTypeDisplayName'), falling
-- back to the directory's own basename if the VDF is missing or has no
-- "display_name". Deliberately not always the directory's basename: some
-- compatibility-tools.d layouts name the directory differently from the
-- tool itself, and only happen to coincide for builds like GE-Proton, which
-- name their directory the same as the tool.
compatibilityToolName :: FilePath -> IO T.Text
compatibilityToolName path = do
  name <- readVdfDisplayName path
  pure $ if T.null name then T.pack (takeBaseName path) else name

-- | Every currently available compatibility tool's (name, path), already
-- deduplicated per Steam's own "last-registered wins" precedence (see
-- 'dedupToolsByName'). Shared by 'getAvailableRunners' and
-- 'findProtonPathByName'.
findAvailableCompatibilityTools :: IO [(String, FilePath)]
findAvailableCompatibilityTools = do
  home <- getHomeDirectory
  extraPathsEnv <- lookupEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS"
  let searchDirs = compatibilityToolSearchDirs home extraPathsEnv
  toolsPerDir <- mapM findCompatibilityToolsIn searchDirs
  pure $ dedupToolsByName (concat toolsPerDir)

-- | Every runner currently installed on the system. 'ExistingRunner' rather
-- than 'RunnerType' by construction: a runner found by scanning the disk is
-- available, and these are what the runner picker offers.
getAvailableRunners :: IO [ExistingRunner]
getAvailableRunners = do
  sysWine <- findExecutable "wine"
  let wineList = if isJust sysWine then [SystemWine] else []

  tools <- findAvailableCompatibilityTools
  let protonList = [ Proton path | (_, path) <- tools ]

  return (wineList ++ protonList)

-- | Resolves a compatibility tool's name (see 'compatibilityToolName', e.g.
-- "GE-Proton10-25") to its current path, if any tool by that name is
-- currently found. Used to re-resolve a persisted Proton runner's name back
-- to a path on every config load (see "Bottle.Logic.Config".loadBottleConfig)
-- -- Decanter persists the tool's name rather than its path precisely so a
-- moved/reinstalled tool with the same name doesn't require touching every
-- bottle's config.
findProtonPathByName :: T.Text -> IO (Maybe FilePath)
findProtonPathByName name = lookup (T.unpack name) <$> findAvailableCompatibilityTools

-- | Determines the display name for a runner
getRunnerTypeDisplayName :: RunnerType -> IO T.Text
getRunnerTypeDisplayName (Existing SystemWine) = do
    (exitCode, out) <- readProcessStdout (proc "wine" ["--version"])
    case exitCode of
        ExitSuccess -> return $ T.strip $ T.pack $ LBS8.unpack out
        _           -> return "System Wine (Unknown Version)"

getRunnerTypeDisplayName (Existing (Proton path)) = do
    name <- readVdfDisplayName path
    pure $ if T.null name then protonFallbackName path else name

getRunnerTypeDisplayName (Missing MissingSystemWine) =
    pure $ tr "System Wine" <> " - " <> tr "not found"

getRunnerTypeDisplayName (Missing (MissingProton path)) =
    pure $ protonFallbackName path <> " - " <> tr "not found"

-- | Display name for a Proton build whose "compatibilitytool.vdf" has no
-- (usable) "display_name" -- its directory's basename instead, e.g.
-- "Proton (GE-Proton10-25)".
protonFallbackName :: FilePath -> T.Text
protonFallbackName path = "Proton (" <> T.pack (takeBaseName path) <> ")"
