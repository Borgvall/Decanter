{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.BottleSnapshotsView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Control.Monad (void, forM_, when)
import qualified Data.Text as T

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)

-- | Validiert den Snapshot-Namen und aktualisiert UI
validateSnapshotName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateSnapshotName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow
  let status = checkNameValidity nameText
  let valid = status == Valid
  
  #setSensitive createBtn valid
  
  if valid
    then do
      Gtk.widgetRemoveCssClass entryRow "error"
      #setVisible errorLabel False
    else do
      Gtk.widgetAddCssClass entryRow "error"
      let errorMsg = explainNameValid status
      #setLabel errorLabel errorMsg
      #setVisible errorLabel True

-- | Zeigt das Popover zum Erstellen eines Snapshots
-- Wir übergeben den 'parentBtn', an den das Popover angeheftet wird.
showCreateSnapshotPopover :: Gtk.Button -> Bottle -> IO () -> IO ()
showCreateSnapshotPopover parentBtn bottle refreshCallback = do
  
  -- Popover erstellen
  popover <- new Gtk.Popover []
  #setParent popover parentBtn  -- Das Popover gehört visuell zum Button
  
  -- Inhalt des Popovers
  contentBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 12
    , #marginTop := 12
    , #marginBottom := 12
    , #marginStart := 12
    , #marginEnd := 12
    , #widthRequest := 300 -- Eine Mindestbreite sieht besser aus
    ]
  
  -- Titel im Popover (optional, aber hilfreich)
  titleLabel <- new Gtk.Label [ #label := tr "New Snapshot", #cssClasses := ["title-4"], #halign := Gtk.AlignStart ]
  #append contentBox titleLabel

  -- Eingabegruppe
  group <- new Adw.PreferencesGroup []
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry
  #append contentBox group
  
  -- Fehler-Label
  errorLabel <- new Gtk.Label 
    [ #label := "", #cssClasses := ["error"], #visible := False, #halign := Gtk.AlignStart ]
  #append contentBox errorLabel
  
  -- Create Button
  createBtn <- new Gtk.Button 
    [ #label := tr "Create"
    , #cssClasses := ["suggested-action"] 
    , #sensitive := False 
    , #halign := Gtk.AlignEnd
    ]
  #append contentBox createBtn
  
  -- Validierung bei Änderung
  void $ on nameEntry #changed $ validateSnapshotName nameEntry createBtn errorLabel
  
  -- Die eigentliche Logik zum Erstellen
  let doCreate = do
        isValid <- #getSensitive createBtn
        when isValid $ do
            sName <- #getText nameEntry
            #setSensitive createBtn False
            
            async $ do
              res <- try (createSnapshotLogic bottle sName) :: IO (Either SomeException ())
              GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                case res of
                  Right _ -> do
                    #popdown popover -- Popover schließen
                    refreshCallback
                  Left err -> do
                     #setLabel errorLabel (T.pack $ "Error: " ++ show err)
                     #setVisible errorLabel True
                     #setSensitive createBtn True
                return False
            return ()

  -- Klick auf Button löst Erstellung aus
  on createBtn #clicked doCreate
  
  -- Enter im Eingabefeld löst ebenfalls Erstellung aus
  on nameEntry #entryActivated doCreate

  #setChild popover (Just contentBox)
  
  -- Popover anzeigen (GTK4: popup)
  #popup popover

-- | Baut die Snapshot-Liste
buildSnapshotView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO Gtk.Widget
buildSnapshotView window bottle stack = do
  
  -- Outer Container
  outerBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 0 ]
  
  -- Header
  header <- new Adw.HeaderBar []
  
  -- Back Button Logic
  backBtn <- new Gtk.Button [ #iconName := "go-previous-symbolic" ]
  on backBtn #clicked $ do
      let detailViewName = "detail_" <> bottleName bottle
      #setVisibleChildName stack detailViewName
  
  #packStart header backBtn
  
  title <- new Adw.WindowTitle [ #title := tr "Snapshots", #subtitle := bottleName bottle ]
  #setTitleWidget header (Just title)
  
  -- Add Button (hier heften wir später das Popover an)
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #cssClasses := ["suggested-action"] ]
  #packEnd header addBtn
  
  #append outerBox header

  -- Content Area
  scrolled <- new Gtk.ScrolledWindow [ #vexpand := True ]
  clamp <- new Adw.Clamp [ #maximumSize := 600 ]
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"], #marginTop := 20, #marginBottom := 20 ]
  
  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)
  #append outerBox scrolled

  -- Logic to refresh the list
  let refreshList = do
        -- Clear list
        let removeAll = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Just c -> Gtk.listBoxRemove listBox c >> removeAll
                Nothing -> return ()
        removeAll
        
        snaps <- listSnapshots bottle
        
        if null snaps 
           then do
             emptyLabel <- new Gtk.Label [ #label := tr "No snapshots yet.", #cssClasses := ["dim-label"], #marginTop := 20 ]
             #append listBox emptyLabel
           else do
             forM_ snaps $ \s -> do
               let rowTitle = (T.pack . show $ snapshotId s) <> ". " <> snapshotName s
               row <- new Adw.ActionRow [ #title := rowTitle, #subtitle := T.pack (snapshotPath s) ]
               
               icon <- new Gtk.Image [ #iconName := "emblem-readonly-symbolic" ]
               #addPrefix row icon
               
               #append listBox row

  refreshList
  
  -- Connect Add Button -> Popover
  on addBtn #clicked $ showCreateSnapshotPopover addBtn bottle refreshList

  Gtk.toWidget outerBox
