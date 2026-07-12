{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Runner
  ( getAvailableRunners
  , getRunnerTypeDisplayName
  , checkSystemWine32Support
  , getSupportedArchitectures
  ) where

import Bottle.Types
import Data.Vdf (extractDisplayName)
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , listDirectory
    , findExecutable
    , getHomeDirectory
    )
import System.FilePath ((</>), takeBaseName)
import System.Process.Typed
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import Control.Monad (filterM)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS8

getAvailableRunners :: IO [RunnerType]
getAvailableRunners = do
  sysWine <- findExecutable "wine"
  let wineList = if isJust sysWine then [SystemWine] else []

  home <- getHomeDirectory
  let compatDir = home </> ".steam/root/compatibilitytools.d"

  protonList <- do
    exists <- doesDirectoryExist compatDir
    if exists
      then do
        entries <- listDirectory compatDir
        -- Filter: must be a directory AND contain compatibilitytool.vdf
        paths <- filterM (\e -> do
            let fullPath = compatDir </> e
            isDir <- doesDirectoryExist fullPath
            hasVdf <- doesFileExist (fullPath </> "compatibilitytool.vdf")
            return (isDir && hasVdf)
            ) entries
        return [ Proton (compatDir </> p) | p <- paths ]
      else return []

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

-- | Checks whether the system supports 32-bit prefixes.
-- Runs 'WINEARCH=win32 wine --version'; if wine32 is missing, this usually returns ExitCode 1.
checkSystemWine32Support :: IO Bool
checkSystemWine32Support = do
    currentEnv <- getEnvironment
    -- Override WINEARCH, but keep the rest (e.g. PATH)
    let newEnv = ("WINEARCH", "win32") : filter ((/= "WINEARCH") . fst) currentEnv

    let procConfig = setEnv newEnv
                   $ setStderr closed
                   $ setStdout closed
                   $ proc "wine" ["--version"]

    result <- runProcess procConfig
    return (result == ExitSuccess)

-- | Returns a list of the architectures supported by the system.
-- Win64 is assumed to always be available.
getSupportedArchitectures :: IO [Arch]
getSupportedArchitectures = do
    win32Support <- checkSystemWine32Support
    if win32Support
       then return [Win64, Win32]
       else return [Win64]
