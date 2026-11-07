{-# LANGUAGE OverloadedStrings #-}

module Cli
  ( Command(..)
  , Confirmation(..)
  , parseCommand
  , runListBottles
  , runListApps
  , runStart
  , runOpen
  , runListSnapshots
  , runCreateSnapshot
  , runRestoreSnapshot
  , runDeleteSnapshot
  ) where

import Control.Monad (unless)
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
  , launchableBottle
  , explainBlockReason
  )
import Bottle.Logic.Name (parseName, explainInvalidName)
import Bottle.Logic.Programs
  ( findWineStartMenuLnks
  , runFileWithStart
  , runWindowsLnk
  )
import Bottle.Logic.Snapshots
  ( createSnapshotLogic
  , deleteSnapshotLogic
  , explainSnapshotRefError
  , isSnapshotableBottle
  , listSnapshots
  , resolveSnapshotRef
  , restoreSnapshotLogic
  )
import Bottle.Types
  ( Bottle
  , BottleG
  , BottleSnapshot(..)
  , ExistingRunner
  , bottleName
  )

-- | The commands supported by the CLI.
data Command
  = Gui (Maybe Text)
  | ListBottles
  | ListApps Text
  | Start Text Text
  | Open Text FilePath
  | ListSnapshots Text
  | CreateSnapshot Text Text
  | RestoreSnapshot Text Text Confirmation
  | DeleteSnapshot Text Text Confirmation
  deriving (Show, Eq)

-- | Whether the user confirmed a destructive command with @--force@.
--
-- A data type rather than a 'Bool' so the call site says what it means.
-- Unlike 'decanter start', restoring or deleting a snapshot destroys data
-- that cannot be recovered, and the CLI has no confirmation dialog to ask
-- in -- this is the command-line equivalent of the GUI's confirmation step
-- (see "Gui.BottleSnapshotsView").
data Confirmation
  = Confirmed
  | Unconfirmed
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
     <> command "list-snapshots" (info listSnapshotsParser
          (progDesc "List a bottle's snapshots, one per line (id and name, tab-separated)"))
     <> command "create-snapshot" (info createSnapshotParser
          (progDesc "Create a snapshot of a bottle"))
     <> command "restore-snapshot" (info restoreSnapshotParser
          (progDesc "Restore a bottle from a snapshot (needs --force)"))
     <> command "delete-snapshot" (info deleteSnapshotParser
          (progDesc "Delete a snapshot (needs --force)"))
      )

    guiParser = Gui <$> optional bottleArg
    listAppsParser = ListApps <$> bottleArg
    startParser = Start <$> bottleArg <*> appArg
    openParser = Open <$> bottleArg <*> fileArg
    listSnapshotsParser = ListSnapshots <$> bottleArg
    createSnapshotParser = CreateSnapshot <$> bottleArg <*> snapshotNameArg
    restoreSnapshotParser = RestoreSnapshot <$> bottleArg <*> snapshotRefArg <*> forceFlag
    deleteSnapshotParser = DeleteSnapshot <$> bottleArg <*> snapshotRefArg <*> forceFlag

    bottleArg = T.pack <$> argument str
      (metavar "BOTTLE" <> help "Name of the bottle" <> completer bottleCompleter)
    appArg = T.pack <$> argument str
      (metavar "APPLICATION" <> help "Name of the application (start menu entry)" <> completer appCompleter)
    fileArg = argument str
      (metavar "FILE" <> help "Path to the file to open" <> completer (bashCompleter "file"))
    snapshotNameArg = T.pack <$> argument str
      (metavar "NAME" <> help "Name of the new snapshot")
    snapshotRefArg = T.pack <$> argument str
      ( metavar "SNAPSHOT" <> help "Snapshot id or name (see list-snapshots)"
     <> completer snapshotRefCompleter
      )
    forceFlag = flag' Confirmed
      ( long "force" <> short 'f'
     <> help "Confirm the operation even though it cannot be undone"
      )
      <|> pure Unconfirmed

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
  mBottle <- bottleFromCompletionWords ["start"] 1
  case mBottle of
    Nothing -> return []
    Just bottle -> do
      lnkPaths <- findWineStartMenuLnks bottle
      return $ filter (isPrefixOf word) (map takeBaseName lnkPaths)

-- | Shell-completion source for the SNAPSHOT argument of
-- 'restore-snapshot' and 'delete-snapshot'. Offers the ids of the bottle
-- named just before it: the unambiguous handle 'resolveSnapshotRef' accepts,
-- and the first column 'list-snapshots' prints. Names aren't offered --
-- they may contain spaces, which the generated completion scripts' word
-- lists can't carry.
snapshotRefCompleter :: Completer
snapshotRefCompleter = mkCompleter $ \word -> do
  mBottle <- bottleFromCompletionWords ["restore-snapshot", "delete-snapshot"] 1
  case mBottle of
    Nothing -> return []
    Just bottle -> do
      snaps <- listSnapshots bottle
      return $ filter (isPrefixOf word) (map (T.unpack . snapshotRefOf) snaps)

-- | The bottle named by an already-typed BOTTLE argument of "commandNames",
-- recovered from this process's own argv (see 'precedingPositionalArg').
-- Shared by 'appCompleter' and 'snapshotRefCompleter'.
bottleFromCompletionWords :: [String] -> Int -> IO (Maybe Bottle)
bottleFromCompletionWords commandNames position = do
  mBottleName <- precedingPositionalArg commandNames position
  case mBottleName of
    Nothing -> pure Nothing
    Just bottleNm -> do
      bottles <- listExistingBottles
      pure (findBottleByName (T.pack bottleNm) bottles)

-- | The nth positional argument (1-based) that follows any of
-- "commandNames" in this process's own argv, as passed by the generated
-- bash/zsh/fish completion scripts. Flags (anything starting with "-") are
-- skipped, so @restore-snapshot --force <bottle> <TAB>@ still resolves
-- "<bottle>" as the first positional argument.
precedingPositionalArg :: [String] -> Int -> IO (Maybe String)
precedingPositionalArg commandNames position = do
  args <- getArgs
  return $ listToMaybe
         $ drop (position - 1)
         $ filter (not . isPrefixOf "-")
         $ drop 1
         $ dropWhile (`notElem` commandNames) (completionWords args)
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

-- | "bottle" as something that can be launched from, or an abort with
-- 'explainBlockReason' if it currently can't run Windows programs (see
-- 'Bottle.Logic.launchableBottle') -- shared by 'runStart' and 'runOpen' so
-- a stale/removed runner fails with a clear message up front instead of
-- silently doing nothing inside 'runWindowsLnk'/'runFileWithStart'.
requireLaunchable :: Bottle -> IO (BottleG ExistingRunner)
requireLaunchable bottle = do
  launchable <- launchableBottle bottle
  case launchable of
    Right runnable -> pure runnable
    Left reason -> do
      hPutStrLn stderr $ T.unpack (explainBlockReason reason)
      exitFailure

runStart :: Text -> Text -> IO ()
runStart bottleNm appNm = withBottle bottleNm $ \bottle -> do
  runnable <- requireLaunchable bottle
  lnkPaths <- findWineStartMenuLnks runnable
  case findAppLnkByName appNm lnkPaths of
    Nothing -> do
      hPutStrLn stderr $ "Application not found in bottle '" ++ T.unpack bottleNm ++ "': " ++ T.unpack appNm
      exitFailure
    Just lnkPath -> runWindowsLnk runnable lnkPath

runOpen :: Text -> FilePath -> IO ()
runOpen bottleNm filePath = withBottle bottleNm $ \bottle -> do
  runnable <- requireLaunchable bottle
  exists <- doesFileExist filePath
  if exists
    then runFileWithStart runnable filePath
    else do
      hPutStrLn stderr $ "File not found: " ++ filePath
      exitFailure

-- | How a snapshot is addressed on the command line and offered for
-- completion: its id, which unlike its name is unambiguous (see
-- 'Bottle.Logic.Snapshots.resolveSnapshotRef').
snapshotRefOf :: BottleSnapshot -> Text
snapshotRefOf = T.pack . show . snapshotId

-- | Prints a bottle's snapshots as "<id>\t<name>", one per line.
--
-- A bottle that isn't a BTRFS subvolume can't have any snapshots; that is
-- reported on stderr rather than as a failure, since the listing itself
-- still succeeded (and is simply empty).
runListSnapshots :: Text -> IO ()
runListSnapshots name = withBottle name $ \bottle -> do
  snapshotable <- isSnapshotableBottle bottle
  unless snapshotable $
    hPutStrLn stderr $ "Bottle '" ++ T.unpack name
      ++ "' is not a BTRFS subvolume, so it cannot have snapshots."
  snaps <- listSnapshots bottle
  mapM_ (\snap -> TIO.putStrLn (snapshotRefOf snap <> "\t" <> snapshotName snap)) snaps

runCreateSnapshot :: Text -> Text -> IO ()
runCreateSnapshot bottleNm snapNm = withBottle bottleNm $ \bottle -> do
  -- Checked before the name, so a bottle that can't have snapshots at all
  -- says so instead of failing deep inside a btrfs ioctl call.
  snapshotable <- isSnapshotableBottle bottle
  unless snapshotable $ do
    hPutStrLn stderr $ "Bottle '" ++ T.unpack bottleNm
      ++ "' is not a BTRFS subvolume; snapshots are only supported on BTRFS."
    exitFailure
  case parseName snapNm of
    Left reason -> do
      hPutStrLn stderr $ T.unpack (explainInvalidName reason)
      exitFailure
    Right validName -> do
      snapshot <- createSnapshotLogic bottle validName
      TIO.putStrLn $ T.concat
        [ "Created snapshot ", snapshotRefOf snapshot
        , " ('", snapshotName snapshot, "') in bottle '", bottleNm, "'."
        ]

runRestoreSnapshot :: Text -> Text -> Confirmation -> IO ()
runRestoreSnapshot bottleNm snapshotRef confirmation = withBottle bottleNm $ \bottle -> do
  -- Resolved before the confirmation is demanded: "no such snapshot" is the
  -- more useful of the two errors when both apply.
  snapshot <- resolveSnapshotOrFail bottle snapshotRef
  requireConfirmation "restore a snapshot over the bottle's current state" confirmation
  restoreSnapshotLogic bottle snapshot
  TIO.putStrLn $ T.concat
    [ "Restored bottle '", bottleNm, "' from snapshot ", snapshotRefOf snapshot
    , " ('", snapshotName snapshot, "')."
    ]

runDeleteSnapshot :: Text -> Text -> Confirmation -> IO ()
runDeleteSnapshot bottleNm snapshotRef confirmation = withBottle bottleNm $ \bottle -> do
  snapshot <- resolveSnapshotOrFail bottle snapshotRef
  requireConfirmation "delete a snapshot" confirmation
  deleteSnapshotLogic snapshot
  TIO.putStrLn $ T.concat
    [ "Deleted snapshot ", snapshotRefOf snapshot
    , " ('", snapshotName snapshot, "') from bottle '", bottleNm, "'."
    ]

-- | Resolves a SNAPSHOT argument against the bottle's snapshots, or aborts
-- with 'explainSnapshotRefError'. Shared by 'runRestoreSnapshot' and
-- 'runDeleteSnapshot', so both accept the same id-or-name references.
resolveSnapshotOrFail :: Bottle -> Text -> IO BottleSnapshot
resolveSnapshotOrFail bottle ref = do
  snaps <- listSnapshots bottle
  case resolveSnapshotRef ref snaps of
    Right snapshot -> pure snapshot
    Left reason -> do
      hPutStrLn stderr $ T.unpack (explainSnapshotRefError reason)
      exitFailure

-- | Aborts a destructive command unless the user confirmed it with
-- @--force@.
requireConfirmation :: String -> Confirmation -> IO ()
requireConfirmation _ Confirmed = pure ()
requireConfirmation operation Unconfirmed = do
  hPutStrLn stderr $ "Refusing to " ++ operation
    ++ ": this cannot be undone. Re-run with --force to proceed."
  exitFailure
