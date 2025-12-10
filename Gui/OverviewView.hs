{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels #-}

module Gui.OverviewView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import Data.GI.Base
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_)

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)
import Gui.BottleView (buildBottleView) -- Abhängigkeit zur Detailansicht

-- | Baut die Übersichtsseite und gibt das Widget sowie eine Refresh-Funktion zurück
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, IO ())
buildOverviewPage window stack = do
  scrolled <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  #setVexpand scrolled True
  
  clamp <- new Adw.Clamp [ #maximumSize := 600, #tighteningThreshold := 400 ]
  
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"] ]
  
  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)
  
  outerBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #marginTop := 24, #spacing := 12 ]
  
  title <- new Gtk.Label [ #label := tr "Your Bottles", #cssClasses := ["title-2"] ]
  #append outerBox title
  
  #append outerBox scrolled

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
               on row #activated $ do
                 -- Navigation zur Detailansicht (aus Gui.BottleView)
                 detailView <- buildBottleView window b stack refreshAction
                 let viewName = "detail_" <> bottleName b
                 
                 #addNamed stack detailView (Just viewName)
                 #setVisibleChildName stack viewName
               
               #append listBox row
  
  widget <- Gtk.toWidget outerBox
  return (widget, refreshAction)
