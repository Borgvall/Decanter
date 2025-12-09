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
  
  -- WICHTIG: Adwaita Window zu GTK Window casten für Funktionen, die Gtk.Window erwarten
  Just windowAsGtk <- castTo Gtk.Window window

  -- Toolbar View Setup
  content <- new Adw.ToolbarView []
  
  -- Header Bar mit "Add" Button
  header <- new Adw.HeaderBar []
  addBtn <- new Gtk.Button [ #iconName := "list-add-symbolic", #tooltipText := tr "Create new Bottle" ]
  #packEnd header addBtn
  #addTopBar content header

  stack <- new Gtk.Stack []
  
  -- Overview Page erstellen
  -- Hier übergeben wir jetzt das gecastete 'windowAsGtk'
  (overviewWidget, refreshList) <- buildOverviewPage windowAsGtk stack
  #addNamed stack overviewWidget (Just "overview")
  
  #setContent content (Just stack)
  #setContent window (Just content)
  
  -- Event: Neue Bottle erstellen (auch hier windowAsGtk nutzen)
  on addBtn #clicked $ showNewBottleDialog windowAsGtk refreshList
  
  -- Initiales Laden der Liste
  refreshList

  #present window

-- | Baut die Übersichtsseite und gibt eine Funktion zum Aktualisieren der Liste zurück
buildOverviewPage :: Gtk.Window -> Gtk.Stack -> IO (Gtk.Widget, IO ())
buildOverviewPage window stack = do
  -- 1. Scrollbares Fenster (Dies ist der Container, der scrollt)
  scrolled <- new Gtk.ScrolledWindow [ #hscrollbarPolicy := Gtk.PolicyTypeNever ]
  #setVexpand scrolled True -- WICHTIG: Damit die Liste den Platz im Fenster ausfüllt
  
  -- 2. Clamp (Zentriert den Inhalt)
  clamp <- new Adw.Clamp [ #maximumSize := 600, #tighteningThreshold := 400 ]
  
  -- 3. ListBox (Der eigentliche Inhalt)
  listBox <- new Gtk.ListBox [ #selectionMode := Gtk.SelectionModeNone, #cssClasses := ["boxed-list"] ]
  
  -- HIERARCHIE VERBINDEN:
  -- ListBox kommt in den Clamp
  #setChild clamp (Just listBox)
  
  -- Clamp kommt in das ScrolledWindow
  #setChild scrolled (Just clamp)
  
  -- Box um alles zusammenzuhalten (mit etwas Margin)
  outerBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #marginTop := 24, #spacing := 12 ]
  
  title <- new Gtk.Label [ #label := tr "Your Bottles", #cssClasses := ["title-2"] ]
  #append outerBox title
  
  -- Das ScrolledWindow (mit Clamp & ListBox darin) kommt in die OuterBox
  #append outerBox scrolled

  let refreshAction = do
        -- 1. Alte Kinder entfernen
        children <- #observeChildren listBox
        
        -- Rekursive Löschfunktion (Workaround für fehlendes removeAll)
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
               
               icon <- new Gtk.Image [ #iconName := "go-next-symbolic" ]
               #addSuffix row icon
               
               #setActivatableWidget row (Just icon) 
               on row #activated $ do
                 detailView <- buildBottleView window b stack
                 let viewName = "detail_" <> bottleName b
                 
                 -- Prüfen, ob View schon existiert wäre gut, aber Stack überschreibt oder ignoriert Namen meistens.
                 -- Einfachheitshalber fügen wir es hinzu. (In Prod würde man erst prüfen)
                 #addNamed stack detailView (Just viewName)
                 #setVisibleChildName stack viewName
               
               #append listBox row
  
  widget <- Gtk.toWidget outerBox
  return (widget, refreshAction)

-- | Dialog zum Erstellen einer neuen Bottle
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
  
  -- 1. Name Entry
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry
  
  -- 2. Architecture Combo
  archRow <- new Adw.ComboRow [ #title := tr "Architecture" ]
  
  -- FIX: stringListNew erwartet (Maybe [Text]), nicht [Text]
  model <- Gtk.stringListNew (Just $ map (T.pack . show) [Win64, Win32])
  
  #setModel archRow (Just model)
  #add group archRow
  
  #append contentBox group
  
  -- Buttons (Cancel / Create)
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

-- | Die Detailansicht
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO Gtk.Widget
buildBottleView window bottle stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10, #marginTop := 20 ]
  
  headerBox <- new Gtk.Box [ #spacing := 10 ]
  
  title <- new Gtk.Label [ #label := bottleName bottle, #cssClasses := ["title-1"] ]
  #append headerBox title
  #append box headerBox

  let addBtn label tooltip action = do
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip ]
        on btn #clicked action
        #append box btn

  runBtn <- new Gtk.Button 
    [ #label := tr "Run Executable / Installer"
    , #tooltipText := tr "Select .exe or .msi file to run inside this bottle" 
    ]
  on runBtn #clicked $ do
    openExecutableFileDialog window $ runExecutable bottle
  #append box runBtn

  addBtn (tr "Wine Config") (tr "Opens winecfg") (runWineCfg bottle)
  addBtn (tr "Registry Editor") (tr "Opens regedit") (runRegedit bottle)
  addBtn (tr "Uninstaller") (tr "Manage installed programs") (runUninstaller bottle)
  addBtn (tr "Browse Files") (tr "Open drive_c in file manager") (runFileManager bottle)
  
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

-- | Handles the response from the file dialog.
-- Fängt alle Exceptions (einschließlich GError bei Abbruch) ab.
handleFileDialogResponse :: FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse userCallback dialog result = do
    -- Versuche, die Datei zu bekommen, fange dabei ALLE Fehler ab (SomeException)
    fileResult <- try (Gtk.fileDialogOpenFinish dialog result) :: IO (Either SomeException Gio.File)
    
    case fileResult of
        Left err -> do
            -- Fehler (z.B. Abbruch des Dialogs) aufgetreten
            putStrLn $ "File dialog operation cancelled or failed: " ++ show err
            return () -- Wichtig: Normale Beendigung der Funktion bei Abbruch
            
        Right gfile -> do
            -- Erfolgreich eine GFile erhalten
            mpath <- Gio.fileGetPath gfile
            case mpath of
                Just path -> do
                    -- Erfolgreich einen lokalen Pfad erhalten
                    userCallback path
                Nothing -> do
                    -- Ausgewählte Datei ist keine lokale URI (z.B. Netzwerk)
                    uri <- Gio.fileGetUri gfile
                    putStrLn $ "Error: Selected file is not a local path. URI: " ++ T.unpack uri
