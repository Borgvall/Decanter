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
import Cli (Command(..), parseCommand, runListApps, runListBottles, runOpen, runStart)
import Gui.OverviewView (buildOverviewPage, navigateToBottle)

main :: IO ()
main = do
  cmd <- parseCommand
  case cmd of
    Gui mBottleName -> runGuiCommand mBottleName
    ListBottles     -> runListBottles
    ListApps b      -> runListApps b
    Start b a       -> runStart b a
    Open b f        -> runOpen b f

-- | Resolves the optional bottle name from 'decanter gui <bottle name>'.
-- If the bottle doesn't exist, the process aborts before a window is even
-- created.
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

  -- No global ToolbarView/HeaderBar: each stack page builds and manages its
  -- own header instead, so the stack itself is the window's content.
  stack <- new Gtk.Stack [ #transitionType := Gtk.StackTransitionTypeSlideLeftRight ]

  -- Wraps the stack so background-thread failures (snapshot restore, bottle
  -- delete, ...) can surface as a toast instead of only being printed to
  -- the console.
  overlay <- new Adw.ToastOverlay []
  #setChild overlay (Just stack)

  let showError :: Text -> IO ()
      showError msg = do
        toast <- new Adw.Toast [ #title := msg ]
        void $ #addToast overlay toast

  (overviewWidget, refreshList) <- buildOverviewPage windowAsGtk stack showError
  void $ #addNamed stack overviewWidget (Just "overview")

  #setContent window (Just overlay)

  refreshList

  -- With 'decanter gui <bottle name>', switch straight to the detail view
  case mBottle of
    Nothing     -> return ()
    Just bottle -> navigateToBottle windowAsGtk stack showError bottle refreshList

  #present window
