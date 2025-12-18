{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels #-}

module Gui.OverviewView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import Data.GI.Base
import qualified Data.Text as T
import Control.Monad (forM_, void)

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)
import Gui.BottleView (buildBottleView)
import Gui.NewBottleDialog (showNewBottleDialog) -- Import für den Dialog

-- | Baut die Übersichtsseite und gibt das Widget sowie eine Refresh-Funktion zurück
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, IO ())
buildOverviewPage window stack = do
  
  -- 1. Outer Container: Adw.ToolbarView (Standard für Views mit Header)
  toolbarView <- new Adw.ToolbarView []

  -- 2. HeaderBar erstellen
  header <- new Adw.HeaderBar []
  
  -- Fenstertitel Widget (Optional, aber gut für Konsistenz)
  titleWidget <- new Adw.WindowTitle [ #title := "Decanter", #subtitle := tr "Library" ]
  #setTitleWidget header (Just titleWidget)
  
  -- Add Button (War vorher in Main.hs)
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #tooltipText := tr "Create new Bottle" ]
  #packEnd header addBtn
  
  #addTopBar toolbarView header

  -- 3. Content Area
  scrolled <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  #setVexpand scrolled True
  
  clamp <- new Adw.Clamp [ #maximumSize := 600, #tighteningThreshold := 400 ]
  
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"], #marginTop := 20, #marginBottom := 20 ]
  
  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)
  
  -- ScrolledWindow als Content der ToolbarView setzen
  #setContent toolbarView (Just scrolled)

  -- Refresh Logic
  let refreshAction = do
        let removeAll = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Just c -> Gtk.listBoxRemove listBox c >> removeAll
                Nothing -> return ()
        removeAll

        bottles <- listExistingBottles
        
        if null bottles
          then do
            emptyLabel <- new Gtk.Label [ #label := tr "No bottles found. Create one!", #marginTop := 20, #cssClasses := ["dim-label"] ]
            #append listBox emptyLabel
          else do
            forM_ bottles $ \b -> do
               row <- new Adw.ActionRow [ #title := bottleName b, #subtitle := T.pack (bottlePath b) ]
               
               icon <- new Gtk.Image [ #iconName := "go-next-symbolic" ]
               #addSuffix row icon
               
               #setActivatableWidget row (Just icon) 
               void $ on row #activated $ do
                 -- Navigation zur Detailansicht
                 detailView <- buildBottleView window b stack refreshAction
                 let viewName = "detail_" <> bottleName b
                 
                 void $ #addNamed stack detailView (Just viewName)
                 #setVisibleChildName stack viewName
               
               #append listBox row
  
  -- Action für Add Button verbinden
  void $ on addBtn #clicked $ showNewBottleDialog window refreshAction
  
  -- Das Widget zurückgeben (muss gecastet werden, ToolbarView ist ein Widget)
  widget <- Gtk.toWidget toolbarView
  return (widget, refreshAction)
