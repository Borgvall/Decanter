{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Gui.BottleView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import GI.Gio.Callbacks (AsyncReadyCallback)
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr) -- Importiere tr

-- | Zeigt den Bestätigungsdialog zum Löschen einer Bottle
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
  
  let handleAlertDialogResult :: AsyncReadyCallback
      handleAlertDialogResult _dialog result = do
          buttonIndex <- Gtk.alertDialogChooseFinish dialog result
          -- buttonIndex == 1 -> Confirmation to delete the bottle
          if buttonIndex == 1
          then do
              -- Stop showing the bottle view, when deleting the bottle
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
          else return ()
  
  let notCancellable = Nothing :: Maybe Gio.Cancellable
  Gtk.alertDialogChoose dialog (Just parent) notCancellable (Just handleAlertDialogResult)

-- | Erstellt die Detailansicht für eine Bottle
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

-- | Typ für den Datei-Auswahl-Callback
type FileSelectedCallback = FilePath -> IO ()

-- | Öffnet einen Dateidialog für EXEs und MSIs
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

-- | Verarbeitet die Antwort des Dateidialogs
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
