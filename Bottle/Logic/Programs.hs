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
import Logic.SystemTool (runSystemTool)
import System.Process.Typed
import System.Directory (doesDirectoryExist, listDirectory, findExecutable)
import System.FilePath ((</>), takeExtension)
import Control.Exception (throwIO)
import Control.Monad (void, filterM, forM)
import Data.List (isSuffixOf)
import Data.Maybe (isJust)

isWinetricksAvailable :: IO Bool
isWinetricksAvailable = do
    path <- findExecutable "winetricks"
    return (isJust path)

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

runWindowsLnk :: Bottle -> FilePath -> IO ()
runWindowsLnk bottle lnkPath = runCmd bottle "wine" ["start", "/unix", lnkPath]

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
