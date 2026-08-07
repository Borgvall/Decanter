{-# LANGUAGE RecordWildCards #-}

module Bottle.Logic.Programs
  ( runCmd
  , runExecutable
  , runFileWithStart
  , runWindowsLnk
  , runWineCfg
  , runRegedit
  , runUninstaller
  , isWinetricksAvailable
  , runWinetricks
  , runFileManager
  , findWineStartMenuLnks
  ) where

import Bottle.Types
import Bottle.Logic.Process (getMergedWineEnv)
import Bottle.Logic.Runner (EngineFamily(..), engineFamily)
import Logic.SystemTool (runSystemTool)
import System.Process.Typed
import System.Directory (doesDirectoryExist, listDirectory, findExecutable)
import System.FilePath ((</>), takeExtension)
import Control.Exception (throwIO)
import Control.Monad (void, filterM, forM)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)

-- | Whether the "Winetricks" entry should be offered for a bottle: only for
-- System Wine bottles, and only if winetricks is actually installed.
--
-- Deliberately not offered for Proton bottles. The winetricks on PATH drives
-- whatever "wine" it finds there -- the host's System Wine -- so pointing it
-- at a Proton bottle would run the host's Wine against a prefix that Proton's
-- own Wine fork created, without Proton's runtime. Doing it properly means
-- going through "umu-run winetricks", which only accepts a fixed list of
-- verbs (no interactive session) and needs GE-Proton/UMU-Proton; and the
-- runtimes people would reach for are largely what GE-Proton ships anyway,
-- with per-game workarounds already handled by umu's protonfixes. See the
-- Non-Goals section in Readme.md on staying out of install recipes.
--
-- A bottle whose System Wine is currently missing stays in 'WineEngine' and
-- keeps the entry: the GUI disables it there, the same way it treats the
-- winecfg/regedit/uninstaller buttons, instead of hiding it and having it
-- reappear after a Wine update.
isWinetricksAvailable :: Bottle -> IO Bool
isWinetricksAvailable Bottle{..} = case engineFamily runner of
    WineEngine   -> isJust <$> findExecutable "winetricks"
    ProtonEngine -> pure False

-- | Starts winetricks for a bottle. Callers are expected to check
-- 'isWinetricksAvailable' first -- this doesn't re-check the runner, and
-- would drive the host's System Wine against a Proton prefix if called for
-- a Proton bottle.
runWinetricks :: Bottle -> IO ()
runWinetricks bottle = runCmd bottle "winetricks" []

-- | Helper to start processes (asynchronously).
-- Adjusts the command if Proton is used.
runCmd :: Bottle -> String -> [String] -> IO ()
runCmd bottle cmd args = do
  mergedEnv <- getMergedWineEnv bottle

  case runner bottle of
    SystemWine ->
        void $ startProcess $ setEnv mergedEnv $ proc cmd args

    Proton _ -> do
        -- If the command is "wine", replace it with "umu-run".
        -- Other tools (like winetricks) may need separate handling,
        -- but umu-run can often just be prepended to them too.
        let (realCmd, realArgs) = if cmd == "wine"
                                  then ("umu-run", args)
                                  else (cmd, args) -- left unchanged for other tools, for now

        -- TODO: check whether umu-run is on PATH or needs to be configured explicitly
        void $ startProcess $ setEnv mergedEnv $ proc realCmd realArgs

    -- Should never be reached -- callers are expected to check
    -- Bottle.Logic.blockReason first (both the GUI and 'decanter start'/
    -- 'decanter open' do). See Bottle.Types.RunnerMissingError.
    MissingSystemWine -> throwIO (RunnerMissingError (runner bottle))
    MissingProton _   -> throwIO (RunnerMissingError (runner bottle))

runWineCfg :: Bottle -> IO ()
runWineCfg bottle = runCmd bottle "wine" ["winecfg"]

runRegedit :: Bottle -> IO ()
runRegedit bottle = runCmd bottle "wine" ["regedit"]

runUninstaller :: Bottle -> IO ()
runUninstaller bottle = runCmd bottle "wine" ["uninstaller"]

runFileManager :: Bottle -> IO ()
runFileManager Bottle{..} = do
  let driveC = bottlePath </> "drive_c"
  runSystemTool "xdg-open" [driveC]

runExecutable :: Bottle -> FilePath -> IO ()
runExecutable bottle filePath = do
  let ext = takeExtension filePath
  if ext == ".msi" || ext == ".MSI"
    then runCmd bottle "wine" ["msiexec", "/i", filePath]
    else runCmd bottle "wine" [filePath]

runFileWithStart :: Bottle -> FilePath -> IO ()
runFileWithStart bottle path = runCmd bottle "wine" ["start", "/unix", path]

-- | Runs a Windows start-menu shortcut (".lnk"). Same as 'runFileWithStart'
-- -- kept as its own name for readability at call sites that specifically
-- launch a start-menu entry, rather than an arbitrary file.
runWindowsLnk :: Bottle -> FilePath -> IO ()
runWindowsLnk = runFileWithStart

findWineStartMenuLnks :: Bottle -> IO [FilePath]
findWineStartMenuLnks Bottle{..} = do
    let driveC = bottlePath </> "drive_c"
    let commonStartMenu = driveC </> "ProgramData/Microsoft/Windows/Start Menu"
    let usersDir = driveC </> "users"
    usersExist <- doesDirectoryExist usersDir

    userStartMenus <- if usersExist
        then do
            users <- listDirectory usersDir
            return [ usersDir </> u </> "AppData/Roaming/Microsoft/Windows/Start Menu" | u <- users ]
        else return []

    let allSearchPaths = commonStartMenu : userStartMenus
    validPaths <- filterM doesDirectoryExist allSearchPaths
    concat <$> mapM findLnksRecursive validPaths

  where
    findLnksRecursive :: FilePath -> IO [FilePath]
    findLnksRecursive dir = do
        content <- listDirectory dir
        paths <- forM content $ \name -> do
            let path = dir </> name
            isDir <- doesDirectoryExist path
            if isDir
                then findLnksRecursive path
                else if ".lnk" `isSuffixOf` name
                    then return [path]
                    else return []
        return (concat paths)
