{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( Command(..)
  , parseCommand
  , runListBottles
  , runListApps
  , runStart
  , runOpen
  ) where

import Data.List (isPrefixOf, sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

import Bottle.Logic
  ( findAppLnkByName
  , findBottleByName
  , listExistingBottles
  , blockReason
  , explainBlockReason
  )
import Bottle.Logic.Programs
  ( findWineStartMenuLnks
  , runFileWithStart
  , runWindowsLnk
  )
import Bottle.Types (Bottle, bottleName)

-- | The commands supported by the CLI.
data Command
  = Gui (Maybe Text)
  | ListBottles
  | ListApps Text
  | Start Text Text
  | Open Text FilePath
  deriving (Show, Eq)

parseCommand :: IO Command
parseCommand = execParser opts
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "Manage and run Windows applications in Wine bottles"
     <> header "decanter - a Wine prefix manager" )

commandParser :: Parser Command
commandParser = subcommands <|> pure (Gui Nothing)
  where
    subcommands = hsubparser
      ( command "gui" (info guiParser
          (progDesc "Start the graphical user interface (optionally on a given bottle)"))
     <> command "list-bottles" (info (pure ListBottles)
          (progDesc "List all existing bottles, one per line"))
     <> command "list-apps" (info listAppsParser
          (progDesc "List the start menu applications of a bottle, one per line"))
     <> command "start" (info startParser
          (progDesc "Start an application (start menu entry) in a bottle"))
     <> command "open" (info openParser
          (progDesc "Open a file in a bottle's context (via Wine's start.exe)"))
      )

    guiParser = Gui <$> optional bottleArg
    listAppsParser = ListApps <$> bottleArg
    startParser = Start <$> bottleArg <*> appArg
    openParser = Open <$> bottleArg <*> fileArg

    bottleArg = T.pack <$> argument str
      (metavar "BOTTLE" <> help "Name of the bottle" <> completer bottleCompleter)
    appArg = T.pack <$> argument str
      (metavar "APPLICATION" <> help "Name of the application (start menu entry)" <> completer appCompleter)
    fileArg = argument str
      (metavar "FILE" <> help "Path to the file to open" <> completer (bashCompleter "file"))

-- | Shell-completion source for BOTTLE arguments: lists the existing
-- bottles live, filtered by the partial word already typed.
bottleCompleter :: Completer
bottleCompleter = listIOCompleter $
  map T.unpack . sortOn id . map bottleName <$> listExistingBottles

-- | Shell-completion source for the APPLICATION argument of 'start'.
-- 'Completer' only hands us the partial word being completed, not the
-- BOTTLE argument that was already typed before it, so we recover it by
-- inspecting the raw "--bash-completion-word" arguments that the
-- generated shell completion script passes to this very process (the
-- documented optparse-applicative technique for context-sensitive
-- completions).
appCompleter :: Completer
appCompleter = mkCompleter $ \word -> do
  mBottleName <- precedingStartBottleArg
  case mBottleName of
    Nothing -> return []
    Just bottleNm -> do
      bottles <- listExistingBottles
      case findBottleByName (T.pack bottleNm) bottles of
        Nothing -> return []
        Just bottle -> do
          lnkPaths <- findWineStartMenuLnks bottle
          return $ filter (isPrefixOf word) (map takeBaseName lnkPaths)

-- | Extracts the BOTTLE word that follows "start" out of this process's own
-- argv, as passed by the generated bash/zsh/fish completion scripts.
precedingStartBottleArg :: IO (Maybe String)
precedingStartBottleArg = do
  args <- getArgs
  return $ listToMaybe (drop 1 (dropWhile (/= "start") (completionWords args)))
  where
    completionWords ("--bash-completion-word" : w : rest) = w : completionWords rest
    completionWords (_ : rest) = completionWords rest
    completionWords [] = []

bottleNotFound :: Text -> IO a
bottleNotFound name = do
  hPutStrLn stderr $ "Bottle not found: " ++ T.unpack name
  exitFailure

-- | Looks up "name" among the existing bottles and runs "action" on it,
-- or bails with 'bottleNotFound'. Shared by 'runListApps', 'runStart' and
-- 'runOpen' so the lookup-or-fail scaffolding isn't repeated at each call
-- site.
withBottle :: Text -> (Bottle -> IO ()) -> IO ()
withBottle name withFoundBottle = do
  bottles <- listExistingBottles
  case findBottleByName name bottles of
    Nothing     -> bottleNotFound name
    Just bottle -> withFoundBottle bottle

runListBottles :: IO ()
runListBottles = do
  bottles <- listExistingBottles
  mapM_ (TIO.putStrLn . bottleName) (sortOn bottleName bottles)

runListApps :: Text -> IO ()
runListApps name = withBottle name $ \bottle -> do
  lnkPaths <- findWineStartMenuLnks bottle
  mapM_ TIO.putStrLn (sortOn id (map (T.pack . takeBaseName) lnkPaths))

-- | Aborts with 'explainBlockReason' if "bottle" currently can't run
-- Windows programs (see 'Bottle.Logic.blockReason') -- shared by 'runStart'
-- and 'runOpen' so a stale/removed runner fails with a clear message up
-- front instead of silently doing nothing inside 'runWindowsLnk'/
-- 'runFileWithStart'.
abortIfBlocked :: Bottle -> IO ()
abortIfBlocked bottle = do
  mReason <- blockReason bottle
  case mReason of
    Nothing -> pure ()
    Just reason -> do
      hPutStrLn stderr $ T.unpack (explainBlockReason reason)
      exitFailure

runStart :: Text -> Text -> IO ()
runStart bottleNm appNm = withBottle bottleNm $ \bottle -> do
  abortIfBlocked bottle
  lnkPaths <- findWineStartMenuLnks bottle
  case findAppLnkByName appNm lnkPaths of
    Nothing -> do
      hPutStrLn stderr $ "Application not found in bottle '" ++ T.unpack bottleNm ++ "': " ++ T.unpack appNm
      exitFailure
    Just lnkPath -> runWindowsLnk bottle lnkPath

runOpen :: Text -> FilePath -> IO ()
runOpen bottleNm filePath = withBottle bottleNm $ \bottle -> do
  abortIfBlocked bottle
  exists <- doesFileExist filePath
  if exists
    then runFileWithStart bottle filePath
    else do
      hPutStrLn stderr $ "File not found: " ++ filePath
      exitFailure
