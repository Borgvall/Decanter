{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import Data.GI.Base
import Control.Monad (void)

import Gui.OverviewView (buildOverviewPage)
-- Gui.NewBottleDialog wird hier nicht mehr gebraucht, das macht jetzt die OverviewView

main :: IO ()
main = do
  _ <- Gtk.init
  Adw.init
  app <- Adw.applicationNew (Just "com.github.borgvall.Decanter") []
  void $ on app #activate (buildUI app)
  void $ #run app Nothing

buildUI :: Adw.Application -> IO ()
buildUI app = do
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

  #present window
