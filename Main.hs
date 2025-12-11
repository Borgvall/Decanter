{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Monad (void)

import Logic.Translation (tr)
import Gui.OverviewView (buildOverviewPage)
import Gui.NewBottleDialog (showNewBottleDialog)

main :: IO ()
main = do
  _ <- Gtk.init
  Adw.init
  app <- Adw.applicationNew (Just "com.haskell.bottles") []
  on app #activate (buildUI app)
  void $ #run app Nothing

buildUI :: Adw.Application -> IO ()
buildUI app = do
  window <- new Adw.ApplicationWindow [ #application := app, #title := "Haskell Bottles" ]
  set window [#defaultWidth := 640, #defaultHeight := 768 ]
  
  Just windowAsGtk <- castTo Gtk.Window window

  content <- new Adw.ToolbarView []
  
  header <- new Adw.HeaderBar []
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #tooltipText := tr "Create new Bottle" ]
  #packEnd header addBtn
  #addTopBar content header

  stack <- new Gtk.Stack []
  
  (overviewWidget, refreshList) <- buildOverviewPage windowAsGtk stack
  #addNamed stack overviewWidget (Just "overview")
  
  #setContent content (Just stack)
  #setContent window (Just content)
  
  on addBtn #clicked $ showNewBottleDialog windowAsGtk refreshList
  
  refreshList

  #present window
