{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.BottleSnapshotsView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Control.Monad (void, forM_)
import qualified Data.Text as T

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)

-- | Validiert den Snapshot-Namen im Dialog
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

-- | Zeigt den Dialog zum Erstellen eines Snapshots
showCreateSnapshotDialog :: Gtk.Window -> Bottle -> IO () -> IO ()
showCreateSnapshotDialog parent bottle refreshCallback = do
  dialog <- new Gtk.Window 
    [ #transientFor := parent
    , #modal := True
    , #title := tr "New Snapshot"
    , #defaultWidth := 400
    , #resizable := False
    ]
    
  -- REPAIRED: Replaced invalid #marginSub with explicit margins
  contentBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 20
    , #marginTop := 20
    , #marginBottom := 20
    , #marginStart := 20
    , #marginEnd := 20 
    ]
  
  group <- new Adw.PreferencesGroup []
  nameEntry <- new Adw.EntryRow [ #title := tr "Snapshot Name" ]
  #add group nameEntry
  #append contentBox group
  
  errorLabel <- new Gtk.Label 
    [ #label := "", #cssClasses := ["error"], #visible := False, #halign := Gtk.AlignStart, #marginStart := 20 ]
  #append contentBox errorLabel
  
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd, #marginEnd := 20 ]
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"], #sensitive := False ]
  
  -- Validierung wiring
  void $ on nameEntry #changed $ validateSnapshotName nameEntry createBtn errorLabel
  
  on createBtn #clicked $ do
    sName <- #getText nameEntry
    #setSensitive createBtn False
    
    async $ do
      res <- try (createSnapshotLogic bottle sName) :: IO (Either SomeException ())
      GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
        case res of
          Right _ -> do
            #close dialog
            refreshCallback
          Left err -> do
             #setLabel errorLabel (T.pack $ "Error: " ++ show err)
             #setVisible errorLabel True
             #setSensitive createBtn True
        return False
    return ()

  #append btnBox cancelBtn
  #append btnBox createBtn
  #append contentBox btnBox
  
  #setChild dialog (Just contentBox)
  #present dialog

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
      -- Wir nehmen an, dass die Detailansicht "detail_<BottleName>" heißt
      let detailViewName = "detail_" <> bottleName bottle
      #setVisibleChildName stack detailViewName
  
  #packStart header backBtn
  
  title <- new Adw.WindowTitle [ #title := tr "Snapshots", #subtitle := bottleName bottle ]
  #setTitleWidget header (Just title)
  
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
               
               -- Icon indicating read-only
               icon <- new Gtk.Image [ #iconName := "emblem-readonly-symbolic" ]
               #addPrefix row icon
               
               #append listBox row

  -- Initial Load
  refreshList
  
  -- Connect Add Button
  on addBtn #clicked $ showCreateSnapshotDialog window bottle refreshList

  Gtk.toWidget outerBox
