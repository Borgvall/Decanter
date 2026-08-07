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

-- | Starts winetricks for a bottle. Only ever called with 'SystemWine' --
-- 'isWinetricksAvailable' gates the entry on that, and the parameter makes
-- the caller show its work rather than this function trusting a comment.
runWinetricks :: Bottle -> ExistingRunner -> IO ()
runWinetricks bottle r = runCmd bottle r "winetricks" []

-- | Helper to start processes (asynchronously).
-- Adjusts the command if Proton is used.
runCmd :: Bottle -> ExistingRunner -> String -> [String] -> IO ()
runCmd bottle r cmd args = do
  mergedEnv <- getMergedWineEnv bottle r

  case r of
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

runWineCfg :: Bottle -> ExistingRunner -> IO ()
runWineCfg bottle r = runCmd bottle r "wine" ["winecfg"]

runRegedit :: Bottle -> ExistingRunner -> IO ()
runRegedit bottle r = runCmd bottle r "wine" ["regedit"]

runUninstaller :: Bottle -> ExistingRunner -> IO ()
runUninstaller bottle r = runCmd bottle r "wine" ["uninstaller"]

runFileManager :: Bottle -> IO ()
runFileManager Bottle{..} = do
  let driveC = bottlePath </> "drive_c"
  runSystemTool "xdg-open" [driveC]

runExecutable :: Bottle -> ExistingRunner -> FilePath -> IO ()
runExecutable bottle r filePath = do
  let ext = takeExtension filePath
  if ext == ".msi" || ext == ".MSI"
    then runCmd bottle r "wine" ["msiexec", "/i", filePath]
    else runCmd bottle r "wine" [filePath]

runFileWithStart :: Bottle -> ExistingRunner -> FilePath -> IO ()
runFileWithStart bottle r path = runCmd bottle r "wine" ["start", "/unix", path]

-- | Runs a Windows start-menu shortcut (".lnk"). Same as 'runFileWithStart'
-- -- kept as its own name for readability at call sites that specifically
-- launch a start-menu entry, rather than an arbitrary file.
runWindowsLnk :: Bottle -> ExistingRunner -> FilePath -> IO ()
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
