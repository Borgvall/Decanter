{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

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
import Gui.NewBottleDialog (showNewBottleDialog)

-- | Switches the stack to the detail view of the given bottle (building it
-- if needed). Used both from the overview page (clicking a row) and on CLI
-- start via 'decanter gui <bottle name>'.
navigateToBottle :: Gtk.Window -> Gtk.Stack -> Bottle -> IO () -> IO ()
navigateToBottle window stack bottle refreshAction = do
  detailView <- buildBottleView window bottle stack refreshAction
  let viewName = "detail_" <> bottleName bottle

  void $ #addNamed stack detailView (Just viewName)
  #setVisibleChildName stack viewName

-- | Builds the overview page and returns the widget along with a refresh function
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, IO ())
buildOverviewPage window stack = do

  -- Adw.ToolbarView is the standard outer container for views with a header
  toolbarView <- new Adw.ToolbarView []

  header <- new Adw.HeaderBar []

  titleWidget <- new Adw.WindowTitle [ #title := "Decanter", #subtitle := tr "Library" ]
  #setTitleWidget header (Just titleWidget)

  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #tooltipText := tr "Create new Bottle" ]
  #packEnd header addBtn

  #addTopBar toolbarView header

  scrolled <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  #setVexpand scrolled True

  clamp <- new Adw.Clamp [ #maximumSize := 600, #tighteningThreshold := 400 ]

  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"], #marginTop := 20, #marginBottom := 20 ]

  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)

  #setContent toolbarView (Just scrolled)

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
               void $ on row #activated $ navigateToBottle window stack b refreshAction

               #append listBox row

  void $ on addBtn #clicked $ showNewBottleDialog window refreshAction

  -- Cast needed since ToolbarView is a Widget, not returned as one directly
  widget <- Gtk.toWidget toolbarView
  return (widget, refreshAction)
