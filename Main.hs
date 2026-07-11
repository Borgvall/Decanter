{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import Data.GI.Base
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Bottle.Logic (findBottleByName, listExistingBottles)
import Bottle.Types (Bottle)
import Cli (Command(..), parseCommand, runListApps, runListBottles, runStart)
import Gui.OverviewView (buildOverviewPage, navigateToBottle)

main :: IO ()
main = do
  cmd <- parseCommand
  case cmd of
    Gui mBottleName -> runGuiCommand mBottleName
    ListBottles     -> runListBottles
    ListApps b      -> runListApps b
    Start b a       -> runStart b a

-- | Löst den optionalen Bottle-Namen von 'decanter gui <bottle name>' auf.
-- Existiert die Bottle nicht, bricht der Prozess ab, bevor überhaupt ein
-- Fenster erzeugt wird.
runGuiCommand :: Maybe Text -> IO ()
runGuiCommand Nothing = startGui Nothing
runGuiCommand (Just name) = do
  bottles <- listExistingBottles
  case findBottleByName name bottles of
    Nothing -> do
      hPutStrLn stderr $ "Bottle not found: " ++ T.unpack name
      exitFailure
    Just bottle -> startGui (Just bottle)

startGui :: Maybe Bottle -> IO ()
startGui mBottle = do
  _ <- Gtk.init
  Adw.init
  app <- Adw.applicationNew (Just "com.github.borgvall.Decanter") []
  void $ on app #activate (buildUI app mBottle)
  void $ #run app Nothing

buildUI :: Adw.Application -> Maybe Bottle -> IO ()
buildUI app mBottle = do
  window <- new Adw.ApplicationWindow [ #application := app, #title := "Decanter" ]
  set window [#defaultWidth := 640, #defaultHeight := 768 ]

  Just windowAsGtk <- castTo Gtk.Window window

  -- HIER GEÄNDERT: Kein globales ToolbarView/HeaderBar mehr.
  -- Wir erstellen direkt den Stack.
  stack <- new Gtk.Stack [ #transitionType := Gtk.StackTransitionTypeSlideLeftRight ]

  (overviewWidget, refreshList) <- buildOverviewPage windowAsGtk stack
  void $ #addNamed stack overviewWidget (Just "overview")

  -- Der Stack ist jetzt direkt der Inhalt des Fensters
  #setContent window (Just stack)

  -- Initiales Laden
  refreshList

  -- Bei 'decanter gui <bottle name>' direkt zur Detailansicht wechseln
  case mBottle of
    Nothing     -> return ()
    Just bottle -> navigateToBottle windowAsGtk stack bottle refreshList

  #present window
