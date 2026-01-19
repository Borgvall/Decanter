{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Gui.BottleView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import GI.Gio.Callbacks (AsyncReadyCallback)
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import Control.Monad (forM_, when, void)
import System.FilePath (takeBaseName)

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)
import Gui.BottleSnapshotsView (buildSnapshotView)

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
          when (buttonIndex == 1) $ do
              #setVisibleChildName windowStack "overview"
              void $ async $ do
                  res <- try (deleteBottleLogic bottle) :: IO (Either SomeException ())
                  GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    case res of
                      Right _ -> refreshCallback
                      Left err -> putStrLn $ "Error: " ++ show err
                    return False
  
  -- FIX: Explizite Typannotation für Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleAlertDialogResult)

-- | Zeigt den Bestätigungsdialog zum Beenden aller Programme
showKillConfirmationDialog :: Gtk.Window -> Bottle -> IO ()
showKillConfirmationDialog parent bottle = do
  let message = tr "Stop all programs in this bottle?"
  let detail  = tr "This will execute 'wineserver -k' and force all running applications to close. Unsaved data may be lost."
  dialog <- new Gtk.AlertDialog [ #message := message, #detail  := detail, #buttons := [ tr "Cancel", tr "Stop All" ] ]
  let handleResult _ result = do
          buttonIndex <- Gtk.alertDialogChooseFinish dialog result
          when (buttonIndex == 1) $ do
              res <- try (killBottleProcesses bottle) :: IO (Either SomeException ())
              case res of
                  Left err -> putStrLn $ "Error: " ++ show err
                  Right _  -> putStrLn "Processes killed."
  
  -- FIX: Explizite Typannotation für Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleResult)

-- | Ändert den Runner für eine Bottle
changeBottleRunner :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO ()
changeBottleRunner window bottle stack refreshCallback = do
  -- Verfügbare Runner abrufen
  availableRunners <- getAvailableRunners
  if null availableRunners
    then do
      -- Keine Runner verfügbar - Fehlermeldung anzeigen
      errorDialog <- new Gtk.AlertDialog 
        [ #message := tr "No runners available. Please install Wine or Proton."
        , #buttons := [ tr "OK" ]
        ]
      Gtk.alertDialogChoose errorDialog (Just window) 
        (Nothing :: Maybe Gio.Cancellable) 
        (Nothing :: Maybe (AsyncReadyCallback))
    else do
      -- Dialog zum Auswählen des Runners
      runnerDialog <- new Adw.MessageDialog 
        [ #transientFor := window
        , #heading := tr "Change Runner"
        , #body := tr "Select a new runner for this bottle. Changing the runner may affect compatibility with installed programs."
        , #closeResponse := "cancel"
        ]
      
      -- Aktuellen Runner markieren
      let currentRunner = runner bottle
      
      -- Action Row für jeden verfügbaren Runner erstellen
      runnerGroup <- new Adw.PreferencesGroup []
      #setExtraChild runnerDialog $ Just runnerGroup
      
      forM_ availableRunners $ \runnerType -> do
        displayName <- getRunnerTypeDisplayName runnerType
        row <- new Adw.ActionRow 
          [ #title := displayName
          , #subtitle := runnerTypeToString runnerType
          , #activatable := True
          ]
        
        -- Aktuellen Runner markieren
        when (runnerType == currentRunner) $ do
          icon <- new Gtk.Image [ #iconName := "object-select-symbolic", #cssClasses := ["dim-label"] ]
          #addSuffix row icon
        
        void $ on row #activated $ do
          -- Runner ändern
          updatedBottle <- changeBottleRunnerLogic bottle runnerType
          -- Stack-Eintrag neu laden
          reloadBottleView window updatedBottle stack refreshCallback
          #close runnerDialog
        
        #add runnerGroup row
      
      -- Cancel Button
      #addResponse runnerDialog "cancel" (tr "Cancel")
      #setResponseAppearance runnerDialog "cancel" Adw.ResponseAppearanceDefault
      
      #show runnerDialog

-- | Lädt die Bottle-Ansicht neu
reloadBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO ()
reloadBottleView window bottle stack refreshCallback = do
  -- Aktuelle View aus dem Stack entfernen
  let viewName = "bottle_" <> bottleName bottle
  mOldChild <- #getChildByName stack viewName
  case mOldChild of
    Just oldChild -> #remove stack oldChild
    Nothing -> return ()
  
  -- Neue View erstellen und hinzufügen
  newView <- buildBottleView window bottle stack refreshCallback
  #addNamed stack newView (Just viewName)
  #setVisibleChildName stack viewName
  
  -- Übersicht aktualisieren
  refreshCallback

-- | Hilfsfunktion zur Konvertierung von RunnerType zu String
runnerTypeToString :: RunnerType -> T.Text
runnerTypeToString SystemWine = tr "System Wine"
runnerTypeToString (Proton path) = T.pack ("Proton (" ++ takeBaseName path ++ ")")

-- | Erstellt die Detailansicht für eine Bottle
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO Gtk.Widget
buildBottleView window bottle stack refreshCallback = do
  
  -- === KONSISTENZ: Adw.ToolbarView statt Gtk.Box ===
  toolbarView <- new Adw.ToolbarView []

  -- HeaderBar
  header <- new Adw.HeaderBar []
  
  backBtn <- new Gtk.Button [ #iconName := "go-previous-symbolic", #tooltipText := tr "Back to Library" ]
  void $ on backBtn #clicked $ #setVisibleChildName stack "overview"
  #packStart header backBtn
  
  winTitle <- new Adw.WindowTitle [ #title := bottleName bottle, #subtitle := tr "Bottle Details" ]
  #setTitleWidget header (Just winTitle)
  
  #addTopBar toolbarView header

  -- Content Bereich
  scrolledWindow <- new Gtk.ScrolledWindow 
    [ #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #vexpand := True
    ]
  #setContent toolbarView (Just scrolledWindow)
  
  clamp <- new Adw.Clamp 
    [ #maximumSize := 450
    , #tighteningThreshold := 300
    ]
  #setChild scrolledWindow (Just clamp)

  contentBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 10
    , #marginTop := 20
    , #marginBottom := 20
    , #valign := Gtk.AlignStart 
    ]
  #setChild clamp (Just contentBox)
  
  -- NEU: Runner-Information anzeigen mit Änderungs-Button
  runnerSectionBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 8
    , #halign := Gtk.AlignStart
    , #marginBottom := 15
    ]
  #append contentBox runnerSectionBox
  
  runnerInfoBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 2
    , #hexpand := True
    , #halign := Gtk.AlignStart
    ]
  #append runnerSectionBox runnerInfoBox
  
  -- Runner-Text anzeigen
  runnerDisplayName <- getRunnerTypeDisplayName (runner bottle)
  runnerLabel <- new Gtk.Label 
    [ #label := runnerDisplayName
    , #halign := Gtk.AlignStart
    , #cssClasses := ["title-4"]
    ]
  #append runnerInfoBox runnerLabel
  
  -- Architektur anzeigen
  archLabel <- new Gtk.Label 
    [ #label := "Architecture: " <> T.pack (archToString (arch bottle))
    , #cssClasses := ["dim-label", "caption"]
    , #halign := Gtk.AlignStart
    ]
  #append runnerInfoBox archLabel
  
  -- Runner-Typ anzeigen
  runnerTypeLabel <- new Gtk.Label 
    [ #label := runnerTypeToString (runner bottle)
    , #cssClasses := ["dim-label", "caption"]
    , #halign := Gtk.AlignStart
    ]
  #append runnerInfoBox runnerTypeLabel
  
  -- Änderungs-Button
  changeRunnerBtn <- new Gtk.Button 
    [ #iconName := "edit-symbolic"
    , #tooltipText := tr "Change Runner"
    , #valign := Gtk.AlignCenter
    , #cssClasses := ["flat"]
    ]
  void $ on changeRunnerBtn #clicked $ 
    changeBottleRunner window bottle stack refreshCallback
  #append runnerSectionBox changeRunnerBtn
  
  let addBtn label tooltip cssClasses action = do 
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip, #cssClasses := cssClasses, #halign := Gtk.AlignFill ]
        void $ on btn #clicked action
        #append contentBox btn
        return btn

  -- Buttons & Content (Run, DropZone, etc. wie gehabt)
  runBtn <- new Gtk.Button [ #label := tr "Run Executable / Installer", #cssClasses := ["suggested-action", "pill"], #halign := Gtk.AlignFill ]
  void $ on runBtn #clicked $ openExecutableFileDialog window $ runExecutable bottle
  #append contentBox runBtn

  -- Drop Zone
  dropZone <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #cssClasses := ["card", "view"], #heightRequest := 48, #valign := Gtk.AlignStart, #halign := Gtk.AlignFill, #marginTop := 5 ]
  dropContent <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #valign := Gtk.AlignCenter ]
  dropIcon <- new Gtk.Image [ #iconName := "document-open-symbolic", #pixelSize := 32, #cssClasses := ["dim-label"] ]
  dropLabel <- new Gtk.Label [ #label := tr "Drag & Drop files here to open", #cssClasses := ["dim-label", "caption"] ]
  #append dropContent dropIcon >> #append dropContent dropLabel >> #append dropZone dropContent
  
  gTypeFile <- glibType @Gio.File
  dropTarget <- Gtk.dropTargetNew gTypeFile [Gdk.DragActionCopy]
  void $ on dropTarget #drop $ \value _ _ -> do
      maybeFile <- fromGValue @(Maybe Gio.File) value
      case maybeFile of
          Just gFile -> do
              mpath <- Gio.fileGetPath gFile
              case mpath of
                  Just path -> runFileWithStart bottle path >> return True
                  Nothing -> return False
          Nothing -> return False
  #addController dropZone dropTarget
  #append contentBox dropZone

  sep1 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 10, #marginBottom := 10 ]
  #append contentBox sep1
  
  -- Snapshot Button
  supportsSnapshots <- isSnapshotableBottle bottle
  when supportsSnapshots $ do
    snapBtn <- new Gtk.Button [ #cssClasses := ["pill"], #halign := Gtk.AlignFill, #marginBottom := 10 ]
    snapBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignCenter ]
    snapIcon <- new Gtk.Image [ #iconName := "camera-photo-symbolic" ]
    snapLabel <- new Gtk.Label [ #label := tr "Manage Snapshots" ]
    #append snapBox snapIcon >> #append snapBox snapLabel >> #setChild snapBtn (Just snapBox)
    
    void $ on snapBtn #clicked $ do
       snapView <- buildSnapshotView window bottle stack
       let viewName = "snapshots_" <> bottleName bottle
       void $ #addNamed stack snapView (Just viewName)
       #setVisibleChildName stack viewName
    #append contentBox snapBtn
    sepSnap <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginBottom := 10 ]
    #append contentBox sepSnap

  -- Program List
  progSectionBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10 ]
  #append contentBox progSectionBox
  progExpander <- new Gtk.Expander [ #label := tr "Installed Programs", #hexpand := True ]
  #append progSectionBox progExpander
  progBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #marginTop := 10 ]
  #setChild progExpander (Just progBox)

  let clearBox box = do
        mChild <- Gtk.widgetGetFirstChild box
        case mChild of
          Just child -> Gtk.boxRemove box child >> clearBox box
          Nothing -> return ()

  let refreshPrograms = do
        clearBox progBox
        lnkFiles <- findWineStartMenuLnks bottle
        if null lnkFiles
          then do
            emptyLabel <- new Gtk.Label [ #label := tr "No programs found", #cssClasses := ["dim-label"] ]
            #append progBox emptyLabel
          else do
            forM_ lnkFiles $ \path -> do
                let name = T.pack $ takeBaseName path
                progBtn <- new Gtk.Button [ #label := name, #halign := Gtk.AlignFill, #tooltipText := T.pack path ]
                void $ on progBtn #clicked $ runWindowsLnk bottle path
                #append progBox progBtn
            set progExpander [ #expanded := True ]

  refreshBtn <- new Gtk.Button [ #iconName := "view-refresh-symbolic", #tooltipText := tr "Refresh program list", #valign := Gtk.AlignStart ]
  void $ on refreshBtn #clicked refreshPrograms
  #append progSectionBox refreshBtn
  refreshPrograms

  sep2 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 10, #marginBottom := 10 ]
  #append contentBox sep2

  -- Tools
  toolsLabel <- new Gtk.Label [ #label := tr "System Tools", #halign := Gtk.AlignStart, #cssClasses := ["heading"] ]
  #append contentBox toolsLabel
  void $ addBtn (tr "Wine Config") (tr "Opens winecfg") [] (runWineCfg bottle)
  void $ addBtn (tr "Registry Editor") (tr "Opens regedit") [] (runRegedit bottle)
  void $ addBtn (tr "Uninstaller") (tr "Manage installed programs") [] (runUninstaller bottle)
  hasWinetricks <- isWinetricksAvailable
  when hasWinetricks $ void $ addBtn (tr "Winetricks") (tr "Manage packages") [] (runWinetricks bottle)
  void $ addBtn (tr "Browse Files") (tr "Open drive_c") [] (runFileManager bottle)
  
  void $ addBtn (tr "Stop all Programs") (tr "Forcefully close all running processes") ["destructive-action"] $ showKillConfirmationDialog window bottle
  sep3 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 20, #marginBottom := 10 ]
  #append contentBox sep3
  void $ addBtn (tr "Delete Bottle") (tr "Permanently delete this bottle") ["destructive-action"] $ showDeleteConfirmationDialog window stack bottle refreshCallback

  Gtk.toWidget toolbarView

-- ... (openExecutableFileDialog und handleFileDialogResponse bleiben unverändert) ...
type FileSelectedCallback = FilePath -> IO ()

openExecutableFileDialog :: Gtk.Window -> FileSelectedCallback -> IO ()
openExecutableFileDialog parentWindow callback = do
    dialog <- Gtk.fileDialogNew
    Gtk.fileDialogSetTitle dialog (tr "Open Executable or Installer")
    let configureFilter name patterns = do
            filterObj <- Gtk.fileFilterNew
            mapM_ (Gtk.fileFilterAddPattern filterObj) patterns
            Gtk.fileFilterSetName filterObj (Just name)
            return filterObj
    exeFilter <- configureFilter (tr "Windows Executables (*.exe)") ["*.exe", "*.EXE"]
    msiFilter <- configureFilter (tr "Windows Installers (*.msi)") ["*.msi", "*.MSI"]
    gType <- glibType @Gtk.FileFilter
    listStore <- Gio.listStoreNew gType
    Gio.listStoreAppend listStore exeFilter
    Gio.listStoreAppend listStore msiFilter
    Gtk.fileDialogSetFilters dialog $ Just listStore
    cancellable <- Gio.cancellableNew
    Gtk.fileDialogOpen dialog (Just parentWindow) (Just cancellable) (Just $ \_ result -> handleFileDialogResponse callback dialog result)

handleFileDialogResponse :: FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse userCallback dialog result = do
    fileResult <- try (Gtk.fileDialogOpenFinish dialog result) :: IO (Either SomeException Gio.File)
    case fileResult of
        Left err -> putStrLn $ "File dialog failed: " ++ show err
        Right gfile -> do
            mpath <- Gio.fileGetPath gfile
            case mpath of
                Just path -> userCallback path
                Nothing -> putStrLn "Error: Not a local file."
