{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.BottleSnapshotsView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import GI.Gio.Callbacks (AsyncReadyCallback)
import qualified GI.Gdk as Gdk
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Control.Monad (void, forM_, when)
import qualified Data.Text as T
import Data.Text (Text)

import Bottle.Types
import Bottle.Logic
import Bottle.Logic.Snapshots
import Logic.Translation (tr)

validateSnapshotName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateSnapshotName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow
  let status = checkNameValidity nameText
  let valid = status == Valid
  #setSensitive createBtn valid
  if valid
    then Gtk.widgetRemoveCssClass entryRow "error" >> #setVisible errorLabel False
    else Gtk.widgetAddCssClass entryRow "error" >> #setLabel errorLabel (explainNameValid status) >> #setVisible errorLabel True

showCreateSnapshotPopover :: Gtk.Button -> Bottle -> IO () -> IO ()
showCreateSnapshotPopover parentBtn bottle refreshCallback = do
  popover <- new Gtk.Popover []
  #setParent popover parentBtn 
  contentBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 12, #marginTop := 12, #marginBottom := 12, #marginStart := 12, #marginEnd := 12, #widthRequest := 300 ]
  titleLabel <- new Gtk.Label [ #label := tr "New Snapshot", #cssClasses := ["title-4"], #halign := Gtk.AlignStart ]
  #append contentBox titleLabel
  group <- new Adw.PreferencesGroup []
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry >> #append contentBox group
  errorLabel <- new Gtk.Label [ #label := "", #cssClasses := ["error"], #visible := False, #halign := Gtk.AlignStart ]
  #append contentBox errorLabel
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"], #sensitive := False, #halign := Gtk.AlignEnd ]
  #append contentBox createBtn
  void $ on nameEntry #changed $ validateSnapshotName nameEntry createBtn errorLabel
  let doCreate = do
        isValid <- #getSensitive createBtn
        when isValid $ do
            sName <- #getText nameEntry
            #setSensitive createBtn False
            void $ async $ do
              res <- try (createSnapshotLogic bottle sName) :: IO (Either SomeException ())
              GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                case res of
                  Right _ -> #popdown popover >> refreshCallback
                  Left err -> #setLabel errorLabel (T.pack $ "Error: " ++ show err) >> #setVisible errorLabel True >> #setSensitive createBtn True
                return False
  void $ on createBtn #clicked doCreate >> on nameEntry #entryActivated doCreate
  #setChild popover (Just contentBox) >> #popup popover

-- | Confirms before restoring a snapshot, since this overwrites the
-- bottle's current filesystem state and cannot be undone (short of
-- restoring yet another snapshot) -- the same destructive-action
-- confirmation pattern as 'Gui.BottleView.showDeleteConfirmationDialog'.
showRestoreSnapshotConfirmationDialog :: Gtk.Window -> Bottle -> BottleSnapshot -> (Text -> IO ()) -> IO () -> IO ()
showRestoreSnapshotConfirmationDialog parent bottle snap showError onSuccess = do
  let fullMessage = T.concat
        [ tr "Are you sure you want to restore the snapshot '"
        , snapshotName snap
        , tr "'?"
        ]
  let detail = tr "This overwrites the bottle's current state with this snapshot -- any changes made since it was taken will be lost. This cannot be undone."
  dialog <- new Gtk.AlertDialog
    [ #message := fullMessage
    , #detail := detail
    , #buttons := [ tr "Cancel", tr "Restore" ]
    ]
  let handleResult :: AsyncReadyCallback
      handleResult _dialog result = do
        buttonIndex <- Gtk.alertDialogChooseFinish dialog result
        when (buttonIndex == 1) $ do
          void $ async $ do
            res <- try (restoreSnapshotLogic bottle snap) :: IO (Either SomeException ())
            GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              case res of
                Right _ -> onSuccess
                Left err -> showError $ tr "Failed to restore snapshot: " <> T.pack (show err)
              return False

  -- Explicit type annotation to disambiguate Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleResult)

-- | Confirms before deleting a snapshot, since a deleted snapshot cannot
-- be recovered afterwards -- same pattern as
-- 'showRestoreSnapshotConfirmationDialog'.
showDeleteSnapshotConfirmationDialog :: Gtk.Window -> BottleSnapshot -> (Text -> IO ()) -> IO () -> IO ()
showDeleteSnapshotConfirmationDialog parent snap showError onSuccess = do
  let fullMessage = T.concat
        [ tr "Are you sure you want to delete the snapshot '"
        , snapshotName snap
        , tr "'?"
        ]
  let detail = tr "This snapshot cannot be recovered afterwards."
  dialog <- new Gtk.AlertDialog
    [ #message := fullMessage
    , #detail := detail
    , #buttons := [ tr "Cancel", tr "Delete" ]
    ]
  let handleResult :: AsyncReadyCallback
      handleResult _dialog result = do
        buttonIndex <- Gtk.alertDialogChooseFinish dialog result
        when (buttonIndex == 1) $ do
          void $ async $ do
            res <- try (deleteSnapshotLogic snap) :: IO (Either SomeException ())
            GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              case res of
                Right _ -> onSuccess
                Left err -> showError $ tr "Failed to delete snapshot: " <> T.pack (show err)
              return False

  -- Explicit type annotation to disambiguate Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleResult)

createMenuBtn :: Text -> Text -> [Text] -> IO Gtk.Button
createMenuBtn labelText iconName cssClassesList = do
    btn <- new Gtk.Button [ #cssClasses := cssClassesList, #halign := Gtk.AlignFill ]
    box <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 12 ]
    img <- new Gtk.Image [ #iconName := iconName ]
    lbl <- new Gtk.Label [ #label := labelText, #halign := Gtk.AlignStart, #hexpand := True ]
    #append box img >> #append box lbl >> #setChild btn (Just box)
    return btn


-- | Builds the snapshot list.
--
-- "reloadDetailView" is Gui.BottleView.reloadBottleView, already applied to
-- the window, bottle and stack (passed through as a plain IO () instead of
-- imported directly, to avoid a cyclic module import, since Gui.BottleView
-- in turn already imports 'buildSnapshotView'). Called after a successful
-- snapshot restore so the detail view (including the Direct3D wrapper
-- display) reflects the restored filesystem state, instead of just
-- switching back to the old, unchanged view.
buildSnapshotView :: Gtk.Window -> Bottle -> Gtk.Stack -> (Text -> IO ()) -> IO () -> IO Gtk.Widget
buildSnapshotView window bottle stack showError reloadDetailView = do

  -- Adw.ToolbarView instead of Gtk.Box, for consistency with the other views
  toolbarView <- new Adw.ToolbarView []

  header <- new Adw.HeaderBar []
  
  backBtn <- new Gtk.Button [ #iconName := "go-previous-symbolic" ]
  let goBack = #setVisibleChildName stack ("detail_" <> bottleName bottle)
  void $ on backBtn #clicked goBack
  #packStart header backBtn

  title <- new Adw.WindowTitle [ #title := tr "Snapshots", #subtitle := bottleName bottle ]
  #setTitleWidget header (Just title)

  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #cssClasses := ["suggested-action"] ]
  #packEnd header addBtn

  -- Escape or Alt+Left also navigates back, matching the GNOME/browser
  -- convention for "go back" -- see Gui.BottleView.buildBottleView, which
  -- adds the same shortcut for the bottle detail view.
  keyController <- new Gtk.EventControllerKey []
  void $ on keyController #keyPressed $ \keyval _keycode modifiers ->
    if keyval == Gdk.KEY_Escape || (keyval == Gdk.KEY_Left && Gdk.ModifierTypeAltMask `elem` modifiers)
      then goBack >> pure True
      else pure False
  #addController toolbarView keyController
  
  #addTopBar toolbarView header

  scrolled <- new Gtk.ScrolledWindow [ #vexpand := True ]
  #setContent toolbarView (Just scrolled)
  
  clamp <- new Adw.Clamp [ #maximumSize := 600 ]
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"], #marginTop := 20, #marginBottom := 20 ]
  
  #setChild clamp (Just listBox)
  #setChild scrolled (Just clamp)

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

               menuBtn <- new Gtk.MenuButton [ #iconName := "view-more-symbolic", #valign := Gtk.AlignCenter, #cssClasses := ["flat"] ]
               popover <- new Gtk.Popover []
               popBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 6, #marginTop := 6, #marginBottom := 6, #marginStart := 6, #marginEnd := 6 ]
               
               browseBtn <- createMenuBtn (tr "Browse Files") "system-file-manager-symbolic" ["flat"]
               void $ on browseBtn #clicked $ #popdown popover >> openSnapshotFileManager s
               #append popBox browseBtn
               
               sep1 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal ]
               #append popBox sep1
               
               restoreBtn <- createMenuBtn (tr "Restore Bottle") "document-revert-symbolic" ["destructive-action"]
               void $ on restoreBtn #clicked $ do
                   #popdown popover
                   showRestoreSnapshotConfirmationDialog window bottle s showError reloadDetailView
               #append popBox restoreBtn

               deleteBtn <- createMenuBtn (tr "Delete Snapshot") "user-trash-symbolic" ["destructive-action"]
               void $ on deleteBtn #clicked $ do
                   #popdown popover
                   showDeleteSnapshotConfirmationDialog window s showError refreshList
               #append popBox deleteBtn
               
               #setChild popover (Just popBox)
               #setPopover menuBtn (Just popover)
               #addSuffix row menuBtn
               #append listBox row

  refreshList
  void $ on addBtn #clicked $ showCreateSnapshotPopover addBtn bottle refreshList

  Gtk.toWidget toolbarView
