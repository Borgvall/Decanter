{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.FileFilter as FileFilter
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)

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
  gtkWindow <- Gtk.toWindow window
  stack <- new Gtk.Stack []
  
  (overview, _) <- buildOverviewPage gtkWindow stack
  #addNamed stack overview $ Just "overview"
  
  content <- new Adw.ToolbarView []
  #setContent content (Just stack)
  #setContent window (Just content)
  #present window

-- Seite 1: Übersicht
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, Bottle -> IO ())
buildOverviewPage window stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 12, #marginTop := 20 ]
  
  statusLabel <- new Gtk.Label [ #label := tr "Welcome" ]
  
  createBtn <- new Gtk.Button 
    [ #label := tr "Create Demo Bottle" 
    , #tooltipText := tr "Creates a standard win64 prefix in ~/.local/share/bottles"
    ]
  
  on createBtn #clicked $ do
    Gtk.labelSetLabel statusLabel (tr "Creating...")
    async $ do
      let newBottle = Bottle "Demo" "/home/user/.local/share/bottles/Demo" SystemWine Win64
      res <- try (createBottleLogic newBottle) :: IO (Either IOError ())
      
      GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
        case res of
          Right _ -> do
            detailView <- buildBottleView window newBottle stack
            #addNamed stack detailView $ Just "detail_demo"
            #setVisibleChildName stack "detail_demo"
          Left err -> Gtk.labelSetLabel statusLabel (T.pack $ show err)
        return False
    return ()

  #append box statusLabel
  #append box createBtn
  boxAsWidget <- Gtk.toWidget box
  return (boxAsWidget, \_ -> return ())

-- Seite 2: Detailansicht
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO Gtk.Widget
buildBottleView window bottle stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10, #marginTop := 20 ]
  
  -- Titel
  title <- new Gtk.Label [ #label := bottleName bottle, #cssClasses := ["title-1"] ]
  #append box title

  -- Helper für Buttons mit Tooltips
  let addBtn label tooltip action = do
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip ]
        on btn #clicked action
        #append box btn

  -- 1. Installer / Runner
  runBtn <- new Gtk.Button 
    [ #label := tr "Run Executable / Installer"
    , #tooltipText := tr "Select .exe or .msi file to run inside this bottle" 
    ]
  on runBtn #clicked $ do
    openExecutableFileDialog window $ runExecutable bottle
  #append box runBtn

  -- 2. Tools
  addBtn (tr "Wine Config") (tr "Opens winecfg") (runWineCfg bottle)
  addBtn (tr "Registry Editor") (tr "Opens regedit") (runRegedit bottle)
  addBtn (tr "Uninstaller") (tr "Manage installed programs") (runUninstaller bottle)
  addBtn (tr "Browse Files") (tr "Open drive_c in file manager") (runFileManager bottle)

  -- Zurück
  backBtn <- new Gtk.Button [ #label := tr "Back", #tooltipText := tr "Return to list" ]
  on backBtn #clicked $ #setVisibleChildName stack "overview"
  #append box backBtn

  Gtk.toWidget box


-- | Callback type that processes the selected file path.
-- This will be called on the GTK main thread.
type FileSelectedCallback = FilePath -> IO ()

-- | Opens a file dialog configured to select single .exe or .msi files.
-- The selected file path is passed to the provided callback.
-- This uses the modern GtkFileDialog API (GTK 4.10+).
openExecutableFileDialog :: Gtk.Window -> FileSelectedCallback -> IO ()
openExecutableFileDialog parentWindow callback = do
    -- 1. Create a new FileDialog
    dialog <- Gtk.fileDialogNew

    -- 2. Set the dialog title
    Gtk.fileDialogSetTitle dialog (tr "Open Executable or Installer")

    -- 3. Create and set file filters to accept only .exe and .msi files
    let -- Helper to create and configure a filter
        configureFilter :: Text -> [Text] -> IO Gtk.FileFilter
        configureFilter name patterns = do
            filterObj <- Gtk.fileFilterNew
            mapM_ (Gtk.fileFilterAddPattern filterObj) patterns
            Gtk.fileFilterSetName filterObj (Just name)
            return filterObj

        configureFilters = do
            -- Create filters for executables and installers
            exeFilter <- configureFilter (tr "Windows Executables (*.exe)") ["*.exe", "*.EXE"]
            msiFilter <- configureFilter (tr "Windows Installers (*.msi)") ["*.msi", "*.MSI"]

            gType <- glibType @Gtk.FileFilter
            listStore <- Gio.listStoreNew gType
            Gio.listStoreAppend listStore exeFilter
            Gio.listStoreAppend listStore msiFilter

            -- Apply the filters to the dialog (user can choose which to use)
            Gtk.fileDialogSetFilters dialog $ Just listStore

    configureFilters

    -- 4. Open the dialog (non-blocking)
    cancellable <- Gio.cancellableNew
    Gtk.fileDialogOpen dialog (Just parentWindow) (Just cancellable) (Just $ \_ result -> handleFileDialogResponse callback dialog result)

-- | Handles the response from the file dialog.
-- Extracts the selected GFile, converts it to a FilePath, and calls the user's callback.
handleFileDialogResponse :: FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse userCallback dialog result = do
    -- Finish the asynchronous open operation to get the selected GFile
    gfile <- Gtk.fileDialogOpenFinish dialog result
    -- Convert the GFile to a local file path (String)
    mpath <- Gio.fileGetPath gfile
    case mpath of
        Just path -> do
            -- Successfully got a path - call the user's callback
            userCallback path
        Nothing -> do
            -- Selected file might be a non-local URI (e.g., on a network)
            uri <- Gio.fileGetUri gfile
            putStrLn $ "Error: Selected file is not a local path. URI: " ++ show uri
