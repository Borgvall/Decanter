{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
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
  
  -- Toolbar View Setup
  content <- new Adw.ToolbarView []
  
  -- Header Bar mit "Add" Button
  header <- new Adw.HeaderBar []
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #tooltipText := tr "Create new Bottle" ]
  #packEnd header addBtn
  #addTopBar content header

  stack <- new Gtk.Stack []
  
  -- Overview Page erstellen
  (overviewWidget, refreshList) <- buildOverviewPage window stack
  #addNamed stack overviewWidget (Just "overview")
  
  #setContent content (Just stack)
  #setContent window (Just content)
  
  -- Event: Neue Bottle erstellen
  on addBtn #clicked $ showNewBottleDialog window refreshList
  
  -- Initiales Laden der Liste
  refreshList

  #present window

-- | Baut die Übersichtsseite und gibt eine Funktion zum Aktualisieren der Liste zurück
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, IO ())
buildOverviewPage window stack = do
  -- Scrollbares Fenster für die Liste
  scrolled <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  
  -- Clamp für schöne Zentrierung (Adwaita Style)
  clamp <- new Adw.Clamp [ #maximumSize := 600, #tighteningThreshold := 400 ]
  #setChild scrolled clamp
  
  -- Container für die Liste
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"] ]
  #setChild scrolled (Just listBox)
  
  -- Box um alles zusammenzuhalten (mit etwas Margin)
  outerBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #marginTop := 24, #spacing := 12 ]
  
  title <- new Gtk.Label [ #label := tr "Your Bottles", #cssClasses := ["title-2"] ]
  #append outerBox title
  #append outerBox clamp

  let refreshAction = do
        -- 1. Alte Kinder entfernen
        children <- #observeChildren listBox
        n <- Gio.listModelGetNChildren children
        -- Ein naiver Weg, alles zu löschen (von hinten nach vorne oder via removeAll wenn verfügbar)
        -- Da ListBox kein 'removeAll' hat, iterieren wir:
        -- (In GTK4 ist das etwas mühsam, wir bauen einfach die Items neu auf und ersetzen sie nicht, 
        -- aber hier simulieren wir ein "Rebuild" indem wir ein neues Model setzen oder manuell löschen.
        -- Einfacher Hack: Wir entfernen alle Rows.)
        let removeAll = do
              child <- Gtk.widgetGetFirstChild listBox
              case child of
                Just c -> Gtk.listBoxRemove listBox c >> removeAll
                Nothing -> return ()
        removeAll

        -- 2. Bottles laden
        bottles <- listExistingBottles
        
        if null bottles
          then do
            emptyLabel <- new Gtk.Label [ #label := tr "No bottles found. Create one!", #marginTop := 20, #cssClasses := ["dim-label"] ]
            #append listBox emptyLabel
          else do
            forM_ bottles $ \b -> do
               row <- new Adw.ActionRow [ #title := bottleName b, #subtitle := T.pack (bottlePath b) ]
               
               -- Pfeil Icon rechts
               icon <- new Gtk.Image [ #iconName := "go-next-symbolic" ]
               #addSuffix row icon
               
               -- Klickbar machen
               #setActivatableWidget row (Just icon) -- Trick: Row wird klickbar
               on row #activated $ do
                 detailView <- buildBottleView window b stack
                 -- Check if detail view already exists to avoid dupes? For now just add unique name.
                 let viewName = "detail_" <> bottleName b
                 #addNamed stack detailView (Just viewName)
                 #setVisibleChildName stack viewName
               
               #append listBox row
  
  widget <- Gtk.toWidget outerBox
  return (widget, refreshAction)

-- | Dialog zum Erstellen einer neuen Bottle
showNewBottleDialog :: Gtk.Window -> IO () -> IO ()
showNewBottleDialog parent refreshCallback = do
  -- Wir nutzen ein GtkWindow als Dialog
  dialog <- new Gtk.Window 
    [ #transientFor := parent
    , #modal := True
    , #title := tr "New Bottle"
    , #defaultWidth := 400
    , #defaultHeight := 300
    , #resizable := False
    ]
  
  contentBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 20, #marginTop := 20, #marginBottom := 20, #marginStart := 20, #marginEnd := 20 ]
  
  -- Preferences Group für Eingaben (Adwaita Style)
  group <- new Adw.PreferencesGroup []
  
  -- 1. Name Entry
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry
  
  -- 2. Architecture Combo
  archRow <- new Adw.ComboRow [ #title := tr "Architecture" ]
  model <- Gtk.stringListNew (map (T.pack . show) [Win64, Win32])
  #setModel archRow (Just model)
  #add group archRow
  
  #append contentBox group
  
  -- Buttons (Cancel / Create)
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]
  
  -- Status Label für Spinner/Text
  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  on createBtn #clicked $ do
    name <- Adw.preferencesRowGetTitle nameEntry -- EntryRow title is the label, getting text is different
    -- AdwEntryRow stores text in the editable part.
    -- Wait, AdwEntryRow inherits from AdwPreferencesRow. To get text:
    nameText <- #getText nameEntry
    
    if T.null nameText 
      then return () -- TODO: Show error
      else do
        -- Disable UI
        #setSensitive createBtn False
        #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
        #setVisible statusLabel True
        
        -- Arch auslesen
        selectedIdx <- #getSelected archRow
        let selectedArch = if selectedIdx == 0 then Win64 else Win32
        
        async $ do
          -- Bottle Objekt erzeugen (Pfad berechnen)
          bottleObj <- createBottleObject nameText selectedArch
          
          -- Anlegen
          res <- try (createBottleLogic bottleObj) :: IO (Either IOError ())
          
          GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
             case res of
               Right _ -> do
                 #close dialog
                 refreshCallback -- Liste im Hauptfenster aktualisieren
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

-- | Die Detailansicht (unverändert zur Vorversion, nur Stack Navigation)
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO Gtk.Widget
buildBottleView window bottle stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10, #marginTop := 20 ]
  
  -- Header mit Zurück-Button Logik in eigener Toolbar wäre schöner, aber Box tut es auch
  headerBox <- new Gtk.Box [ #spacing := 10 ]
  
  title <- new Gtk.Label [ #label := bottleName bottle, #cssClasses := ["title-1"] ]
  #append headerBox title
  #append box headerBox

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
  
  -- Zurück Button (Wichtig: Stack Navigation)
  backBtn <- new Gtk.Button [ #label := tr "Back to Library", #marginTop := 20 ]
  on backBtn #clicked $ #setVisibleChildName stack "overview"
  #append box backBtn

  Gtk.toWidget box

-- FILE DIALOG (unverändert, siehe dein Upload)
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
            -- ACHTUNG: Hier war vorher der Fehler. Wir nutzen listStoreNew OHNE gType Argument in neueren Bindings
            -- oder wir nutzen explizite Typannotationen. 
            -- Da 'gType' im vorherigen Code ein Problem war, nutzen wir den Weg, der in Haskell-GI meist funktioniert:
            listStore <- Gio.listStoreNew -- Type inference should work if we append typed items
            Gio.listStoreAppend listStore exeFilter
            Gio.listStoreAppend listStore msiFilter
            Gtk.fileDialogSetFilters dialog $ Just listStore

    configureFilters
    cancellable <- Gio.cancellableNew
    Gtk.fileDialogOpen dialog (Just parentWindow) (Just cancellable) (Just $ \_ result -> handleFileDialogResponse callback dialog result)

handleFileDialogResponse :: FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse userCallback dialog result = do
    gfile <- Gtk.fileDialogOpenFinish dialog result
    mpath <- Gio.fileGetPath gfile
    case mpath of
        Just path -> userCallback path
        Nothing -> return ()
