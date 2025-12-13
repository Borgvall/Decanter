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
import Data.Text (Text)

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
showCreateSnapshotPopover :: Gtk.Button -> Bottle -> IO () -> IO ()
showCreateSnapshotPopover parentBtn bottle refreshCallback = do
  popover <- new Gtk.Popover []
  #setParent popover parentBtn 
  
  contentBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 12
    , #marginTop := 12
    , #marginBottom := 12
    , #marginStart := 12
    , #marginEnd := 12
    , #widthRequest := 300 
    ]
  
  titleLabel <- new Gtk.Label [ #label := tr "New Snapshot", #cssClasses := ["title-4"], #halign := Gtk.AlignStart ]
  #append contentBox titleLabel

  group <- new Adw.PreferencesGroup []
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry
  #append contentBox group
  
  errorLabel <- new Gtk.Label 
    [ #label := "", #cssClasses := ["error"], #visible := False, #halign := Gtk.AlignStart ]
  #append contentBox errorLabel
  
  createBtn <- new Gtk.Button 
    [ #label := tr "Create"
    , #cssClasses := ["suggested-action"] 
    , #sensitive := False 
    , #halign := Gtk.AlignEnd
    ]
  #append contentBox createBtn
  
  void $ on nameEntry #changed $ validateSnapshotName nameEntry createBtn errorLabel
  
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
                    #popdown popover 
                    refreshCallback
                  Left err -> do
                     #setLabel errorLabel (T.pack $ "Error: " ++ show err)
                     #setVisible errorLabel True
                     #setSensitive createBtn True
                return False
            return ()

  on createBtn #clicked doCreate
  on nameEntry #entryActivated doCreate

  #setChild popover (Just contentBox)
  #popup popover

-- | Hilfsfunktion: Erstellt einen Button mit Icon und Text (manuell via Box)
createMenuBtn :: Text -> Text -> [Text] -> IO Gtk.Button
createMenuBtn labelText iconName cssClassesList = do
    btn <- new Gtk.Button 
        [ #cssClasses := cssClassesList 
        , #halign := Gtk.AlignFill
        ]
    
    box <- new Gtk.Box 
        [ #orientation := Gtk.OrientationHorizontal
        , #spacing := 12 
        ]
    
    img <- new Gtk.Image [ #iconName := iconName ]
    lbl <- new Gtk.Label [ #label := labelText, #halign := Gtk.AlignStart, #hexpand := True ]
    
    #append box img
    #append box lbl
    
    #setChild btn (Just box)
    return btn

-- | Baut die Snapshot-Liste
buildSnapshotView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO Gtk.Widget
buildSnapshotView window bottle stack = do
  
  outerBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 0 ]
  
  header <- new Adw.HeaderBar []
  
  backBtn <- new Gtk.Button [ #iconName := "go-previous-symbolic" ]
  on backBtn #clicked $ do
      let detailViewName = "detail_" <> bottleName bottle
      #setVisibleChildName stack detailViewName
  
  #packStart header backBtn
  
  title <- new Adw.WindowTitle [ #title := tr "Snapshots", #subtitle := bottleName bottle ]
  #setTitleWidget header (Just title)
  
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #cssClasses := ["suggested-action"] ]
  #packEnd header addBtn
  
  #append outerBox header

  scrolled <- new Gtk.ScrolledWindow [ #vexpand := True ]
  clamp <- new Adw.Clamp [ #maximumSize := 600 ]
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"], #marginTop := 20, #marginBottom := 20 ]
  
  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)
  #append outerBox scrolled

  let refreshList = do
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

               -- Menu Button
               menuBtn <- new Gtk.MenuButton 
                    [ #iconName := "view-more-symbolic"
                    , #valign := Gtk.AlignCenter
                    , #cssClasses := ["flat"] 
                    ]
               
               popover <- new Gtk.Popover []
               
               popBox <- new Gtk.Box 
                    [ #orientation := Gtk.OrientationVertical
                    , #spacing := 6
                    , #marginTop := 6
                    , #marginBottom := 6
                    , #marginStart := 6
                    , #marginEnd := 6 
                    ]
               
               -- 1. Browse Files (FIXED: Mit createMenuBtn)
               browseBtn <- createMenuBtn (tr "Browse Files") "system-file-manager-symbolic" ["flat"]
               on browseBtn #clicked $ do
                   #popdown popover
                   openSnapshotFileManager s
               #append popBox browseBtn
               
               -- Separator
               sep1 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]
               #append popBox sep1
               
               -- 2. Restore (FIXED: Mit createMenuBtn)
               restoreBtn <- createMenuBtn (tr "Restore Bottle") "document-revert-symbolic" ["destructive-action"]
               on restoreBtn #clicked $ do
                   #popdown popover
                   putStrLn "Starting restore..."
                   async $ do
                       res <- try (restoreSnapshotLogic bottle s) :: IO (Either SomeException ())
                       GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                           case res of
                               Right _ -> do
                                   let detailViewName = "detail_" <> bottleName bottle
                                   #setVisibleChildName stack detailViewName
                               Left err -> do
                                   putStrLn $ "Error during restore: " ++ show err
                           return False
                   return ()

               #append popBox restoreBtn
               
               -- 3. Delete Snapshot (FIXED: Mit createMenuBtn)
               deleteBtn <- createMenuBtn (tr "Delete Snapshot") "user-trash-symbolic" ["destructive-action"]
               on deleteBtn #clicked $ do
                   #popdown popover
                   async $ do
                       deleteSnapshotLogic s
                       GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                           refreshList 
                           return False
                   return ()

               #append popBox deleteBtn
               
               #setChild popover (Just popBox)
               #setPopover menuBtn (Just popover)
               
               #addSuffix row menuBtn
               #append listBox row

  refreshList
  
  on addBtn #clicked $ showCreateSnapshotPopover addBtn bottle refreshList

  Gtk.toWidget outerBox
