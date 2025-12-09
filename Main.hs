{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void, forM_)
import Data.Maybe (fromMaybe)

import Bottle.Types
import Bottle.Logic

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
                 detailView <- buildBottleView window b stack refreshAction
                 let viewName = "detail_" <> bottleName b
                 
                 #addNamed stack detailView (Just viewName)
                 #setVisibleChildName stack viewName
               
               #append listBox row
  
  widget <- Gtk.toWidget outerBox
  return (widget, refreshAction)

showNewBottleDialog :: Gtk.Window -> IO () -> IO ()
showNewBottleDialog parent refreshCallback = do
  dialog <- new Gtk.Window 
    [ #transientFor := parent
    , #modal := True
    , #title := tr "New Bottle"
    , #defaultWidth := 400
    , #defaultHeight := 300
    , #resizable := False
    ]
  
  contentBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 20, #marginTop := 20, #marginBottom := 20, #marginStart := 20, #marginEnd := 20 ]
  
  group <- new Adw.PreferencesGroup []
  
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry
  
  archRow <- new Adw.ComboRow [ #title := tr "Architecture" ]
  
  model <- Gtk.stringListNew (Just $ map (T.pack . show) [Win64, Win32])
  
  #setModel archRow (Just model)
  #add group archRow
  
  #append contentBox group
  
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]
  
  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  on createBtn #clicked $ do
    nameText <- #getText nameEntry
    
    if T.null nameText 
      then return () 
      else do
        #setSensitive createBtn False
        #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
        #setVisible statusLabel True
        
        selectedIdx <- #getSelected archRow
        let selectedArch = if selectedIdx == 0 then Win64 else Win32
        
        async $ do
          bottleObj <- createBottleObject nameText selectedArch
          res <- try (createBottleLogic bottleObj) :: IO (Either IOError ())
          
          GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
             case res of
               Right _ -> do
                 #close dialog
                 refreshCallback 
               Left err -> do
                 #setLabel statusLabel (T.pack $ "Error: " ++ show err)
                 #setSensitive createBtn True
             return False
        return ()

  #append btnBox cancelBtn
  #append btnBox createBtn
  
  #append contentBox statusLabel
  #append contentBox btnBox
  
  #setChild dialog (Just contentBox)
  #present dialog

showDeleteConfirmationDialog :: Gtk.Window -> Gtk.Stack -> Bottle -> IO () -> IO ()
showDeleteConfirmationDialog parent windowStack bottle refreshCallback = do
  let fullMessage = T.concat 
        [ tr "Are you sure you want to delete the bottle '"
        , bottleName bottle
        , tr "'? All data will be lost. This cannot be undone."
        ]
        
  dialog <- new Gtk.AlertDialog 
    [ #message := fullMessage
    , #buttons := [ tr "Cancel", tr "Delete" ]
    ]

  #show dialog (Just parent) Nothing $ Just $ \_ result -> do -- FIX: Fügen Sie Nothing für Gio.Cancellable ein
    if result == 1
      then do
        #setVisibleChildName windowStack "overview"
        
        async $ do
          res <- try (deleteBottleLogic bottle) :: IO (Either SomeException ())
          
          GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
            case res of
              Right _ -> do
                putStrLn $ "Bottle " ++ T.unpack (bottleName bottle) ++ " erfolgreich gelöscht."
                refreshCallback
              Left err -> do
                putStrLn $ "Fehler beim Löschen der Bottle: " ++ show err
            return False
        return ()
      else 
        return ()

buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO Gtk.Widget
buildBottleView window bottle stack refreshCallback = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10, #marginTop := 20 ]
  
  headerBox <- new Gtk.Box [ #spacing := 10 ]
  
  title <- new Gtk.Label [ #label := bottleName bottle, #cssClasses := ["title-1"] ]
  #append headerBox title
  #append box headerBox

  let addBtn label tooltip cssClasses action = do 
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip, #cssClasses := cssClasses ]
        on btn #clicked action
        #append box btn
        return btn

  runBtn <- new Gtk.Button 
    [ #label := tr "Run Executable / Installer"
    , #tooltipText := tr "Select .exe or .msi file to run inside this bottle" 
    ]
  on runBtn #clicked $ do
    openExecutableFileDialog window $ runExecutable bottle
  #append box runBtn

  addBtn (tr "Wine Config") (tr "Opens winecfg") [] (runWineCfg bottle)
  addBtn (tr "Registry Editor") (tr "Opens regedit") [] (runRegedit bottle)
  addBtn (tr "Uninstaller") (tr "Manage installed programs") [] (runUninstaller bottle)
  addBtn (tr "Browse Files") (tr "Open drive_c in file manager") [] (runFileManager bottle)
  
  addBtn (tr "Delete Bottle") (tr "Permanently delete this bottle and all its contents") ["destructive-action"] $ do
    showDeleteConfirmationDialog window stack bottle refreshCallback

  backBtn <- new Gtk.Button [ #label := tr "Back to Library", #marginTop := 20 ]
  on backBtn #clicked $ #setVisibleChildName stack "overview"
  #append box backBtn

  Gtk.toWidget box

type FileSelectedCallback = FilePath -> IO ()

openExecutableFileDialog :: Gtk.Window -> FileSelectedCallback -> IO ()
openExecutableFileDialog parentWindow callback = do
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog (tr "Open Executable or Installer")

    let configureFilter :: Text -> [Text] -> IO Gtk.FileFilter
        configureFilter name patterns = do
            filterObj <- Gtk.fileFilterNew
            mapM_ (Gtk.fileFilterAddPattern filterObj) patterns
            Gtk.fileFilterSetName filterObj (Just name)
            return filterObj

        configureFilters = do
            exeFilter <- configureFilter (tr "Windows Executables (*.exe)") ["*.exe", "*.EXE"]
            msiFilter <- configureFilter (tr "Windows Installers (*.msi)") ["*.msi", "*.MSI"]
            
            gType <- glibType @Gtk.FileFilter
            listStore <- Gio.listStoreNew gType
            
            Gio.listStoreAppend listStore exeFilter
            Gio.listStoreAppend listStore msiFilter
            Gtk.fileDialogSetFilters dialog $ Just listStore

    configureFilters
    cancellable <- Gio.cancellableNew
    Gtk.fileDialogOpen dialog (Just parentWindow) (Just cancellable) (Just $ \_ result -> handleFileDialogResponse callback dialog result)

handleFileDialogResponse :: FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse userCallback dialog result = do
    fileResult <- try (Gtk.fileDialogOpenFinish dialog result) :: IO (Either SomeException Gio.File)
    
    case fileResult of
        Left err -> do
            putStrLn $ "File dialog operation cancelled or failed: " ++ show err
            return ()
            
        Right gfile -> do
            mpath <- Gio.fileGetPath gfile
            case mpath of
                Just path -> do
                    userCallback path
                Nothing -> do
                    uri <- Gio.fileGetUri gfile
                    putStrLn $ "Error: Selected file is not a local path. URI: " ++ T.unpack uri
