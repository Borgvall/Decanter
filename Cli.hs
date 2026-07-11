{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( Command(..)
  , parseCommand
  , runListBottles
  , runListApps
  , runStart
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import System.IO (hPutStrLn, stderr)

import Bottle.Logic
  ( findAppLnkByName
  , findBottleByName
  , findWineStartMenuLnks
  , listExistingBottles
  , runWindowsLnk
  )
import Bottle.Types (bottleName)

-- | Die von der CLI unterstützten Kommandos.
data Command
  = Gui (Maybe Text)
  | ListBottles
  | ListApps Text
  | Start Text Text
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
      )

    guiParser = Gui <$> optional bottleArg
    listAppsParser = ListApps <$> bottleArg
    startParser = Start <$> bottleArg <*> appArg

    bottleArg = T.pack <$> argument str (metavar "BOTTLE" <> help "Name of the bottle")
    appArg = T.pack <$> argument str (metavar "APPLICATION" <> help "Name of the application (start menu entry)")

bottleNotFound :: Text -> IO a
bottleNotFound name = do
  hPutStrLn stderr $ "Bottle not found: " ++ T.unpack name
  exitFailure

runListBottles :: IO ()
runListBottles = do
  bottles <- listExistingBottles
  mapM_ (TIO.putStrLn . bottleName) (sortOn bottleName bottles)

runListApps :: Text -> IO ()
runListApps name = do
  bottles <- listExistingBottles
  case findBottleByName name bottles of
    Nothing -> bottleNotFound name
    Just bottle -> do
      lnkPaths <- findWineStartMenuLnks bottle
      mapM_ TIO.putStrLn (sortOn id (map (T.pack . takeBaseName) lnkPaths))

runStart :: Text -> Text -> IO ()
runStart bottleNm appNm = do
  bottles <- listExistingBottles
  case findBottleByName bottleNm bottles of
    Nothing -> bottleNotFound bottleNm
    Just bottle -> do
      lnkPaths <- findWineStartMenuLnks bottle
      case findAppLnkByName appNm lnkPaths of
        Nothing -> do
          hPutStrLn stderr $ "Application not found in bottle '" ++ T.unpack bottleNm ++ "': " ++ T.unpack appNm
          exitFailure
        Just lnkPath -> runWindowsLnk bottle lnkPath
