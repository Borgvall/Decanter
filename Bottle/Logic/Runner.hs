{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Runner
  ( getAvailableRunners
  , getRunnerTypeDisplayName
  , compatibilityToolSearchDirs
  , dedupToolsByName
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
import System.Exit (ExitCode(..))
import Control.Monad (filterM)
import Data.List (nubBy)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS8

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
-- subdirectories that contain a "compatibilitytool.vdf".
findCompatibilityToolsIn :: FilePath -> IO [(String, FilePath)]
findCompatibilityToolsIn dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      paths <- filterM (\e -> do
          let fullPath = dir </> e
          isDir <- doesDirectoryExist fullPath
          hasVdf <- doesFileExist (fullPath </> "compatibilitytool.vdf")
          return (isDir && hasVdf)
          ) entries
      return [ (p, dir </> p) | p <- paths ]

getAvailableRunners :: IO [RunnerType]
getAvailableRunners = do
  sysWine <- findExecutable "wine"
  let wineList = if isJust sysWine then [SystemWine] else []

  home <- getHomeDirectory
  extraPathsEnv <- lookupEnv "STEAM_EXTRA_COMPAT_TOOLS_PATHS"
  let searchDirs = compatibilityToolSearchDirs home extraPathsEnv

  toolsPerDir <- mapM findCompatibilityToolsIn searchDirs
  let protonList = [ Proton path | (_, path) <- dedupToolsByName (concat toolsPerDir) ]

  return (wineList ++ protonList)

-- | Determines the display name for a runner
getRunnerTypeDisplayName :: RunnerType -> IO T.Text
getRunnerTypeDisplayName SystemWine = do
    (exitCode, out) <- readProcessStdout (proc "wine" ["--version"])
    case exitCode of
        ExitSuccess -> return $ T.strip $ T.pack $ LBS8.unpack out
        _           -> return "System Wine (Unknown Version)"

getRunnerTypeDisplayName (Proton path) = do
    let vdfPath = path </> "compatibilitytool.vdf"
    exists <- doesFileExist vdfPath
    if exists
        then do
            content <- readFile vdfPath
            let name = extractDisplayName (T.pack content)
            if T.null name
                then return $ "Proton (" <> T.pack (takeBaseName path) <> ")" -- Fallback
                else return name
        else return $ "Proton (" <> T.pack (takeBaseName path) <> ")"

getRunnerTypeDisplayName MissingSystemWine = pure $ tr "System Wine" <> " - " <> tr "not found"

getRunnerTypeDisplayName (MissingProton path) =
    pure $ "Proton (" <> T.pack (takeBaseName path) <> ")" <> " - " <> tr "not found"
