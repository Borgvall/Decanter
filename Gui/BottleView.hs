{-# LANGUAGE LambdaCase, OverloadedStrings, OverloadedLabels, TypeApplications #-}

module Gui.BottleView where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import GI.Gio.Callbacks (AsyncReadyCallback)
import Data.GI.Base
import Data.GI.Base.GValue (fromGValue)
import Control.Concurrent.Async (async)
import Control.Exception (try, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forM_, when, void)
import System.FilePath (takeBaseName)

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

-- | Zeigt den Bestätigungsdialog zum Beenden aller Programme
showKillConfirmationDialog :: Gtk.Window -> Bottle -> IO ()
showKillConfirmationDialog parent bottle = do
  let message = tr "Stop all programs in this bottle?"
  let detail  = tr "This will execute 'wineserver -k' and force all running applications to close. Unsaved data may be lost."
  
  dialog <- new Gtk.AlertDialog 
    [ #message := message
    , #detail  := detail
    , #buttons := [ tr "Cancel", tr "Stop All" ]
    ]
  
  let handleResult :: AsyncReadyCallback
      handleResult _ result = do
          buttonIndex <- Gtk.alertDialogChooseFinish dialog result
          -- buttonIndex == 1 -> Bestätigung ("Stop All")
          if buttonIndex == 1
          then do
              res <- try (killBottleProcesses bottle) :: IO (Either SomeException ())
              case res of
                  Left err -> putStrLn $ "Fehler beim Beenden der Prozesse: " ++ show err
                  Right _  -> putStrLn "Alle Prozesse in der Bottle wurden beendet."
          else return ()
  
  let notCancellable = Nothing :: Maybe Gio.Cancellable
  Gtk.alertDialogChoose dialog (Just parent) notCancellable (Just handleResult)

-- | Erstellt die Detailansicht für eine Bottle
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO Gtk.Widget
buildBottleView window bottle stack refreshCallback = do
  -- 1. Die innere Box für den Inhalt
  contentBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 10
    , #marginTop := 20
    , #marginBottom := 20
    , #valign := Gtk.AlignStart 
    ]
  
  headerBox <- new Gtk.Box [ #spacing := 10, #halign := Gtk.AlignCenter ]
  
  title <- new Gtk.Label 
    [ #label := bottleName bottle
    , #cssClasses := ["title-1"] 
    , #halign := Gtk.AlignCenter
    ]
  #append headerBox title
  #append contentBox headerBox

  let addBtn label tooltip cssClasses action = do 
        btn <- new Gtk.Button 
            [ #label := label
            , #tooltipText := tooltip
            , #cssClasses := cssClasses 
            , #halign := Gtk.AlignFill
            ]
        on btn #clicked action
        #append contentBox btn
        return btn

  -- === 1. Hauptaktion ===
  runBtn <- new Gtk.Button 
    [ #label := tr "Run Executable / Installer"
    , #tooltipText := tr "Select .exe or .msi file to run inside this bottle" 
    , #cssClasses := ["suggested-action", "pill"]
    , #halign := Gtk.AlignFill
    ]
  on runBtn #clicked $ do
    openExecutableFileDialog window $ runExecutable bottle
  #append contentBox runBtn

  -- === DROP ZONE ===
  
  -- 1. Visueller Container (Box mit "card" Style für Rahmen/Hintergrund)
  dropZone <- new Gtk.Box 
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 5
    , #cssClasses := ["card", "view"] -- 'card' gibt den Rahmen, 'view' den Hintergrund
    , #heightRequest := 48
    , #valign := Gtk.AlignStart
    , #halign := Gtk.AlignFill
    , #marginTop := 5
    ]
  
  -- Inhalt der Drop Zone (Icon und Text)
  dropContent <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #valign := Gtk.AlignCenter ]
  
  dropIcon <- new Gtk.Image [ #iconName := "document-open-symbolic", #pixelSize := 32, #cssClasses := ["dim-label"] ]
  dropLabel <- new Gtk.Label [ #label := tr "Drag & Drop files here to open", #cssClasses := ["dim-label", "caption"] ]
  
  #append dropContent dropIcon
  #append dropContent dropLabel
  #append dropZone dropContent
  
  -- 2. Das DropTarget (Die Logik)
  -- Wir akzeptieren Gio.File Objekte (das ist Standard bei Drag&Drop aus dem Dateimanager)
  gTypeFile <- glibType @Gio.File
  dropTarget <- Gtk.dropTargetNew gTypeFile [Gdk.DragActionCopy]
  
  -- Signal-Handler für das Drop-Event
  on dropTarget #drop $ \value _x _y -> do
      -- Das 'value' ist ein generisches GValue. Wir müssen es zu einem Gio.File casten.
      maybeFile <- fromGValue @(Maybe Gio.File) value
      
      case maybeFile of
          Just gFile -> do
              -- Wenn es eine Datei ist, holen wir den Pfad
              maybePath <- Gio.fileGetPath gFile
              case maybePath of
                  Just path -> do
                      putStrLn $ "File dropped: " ++ path
                      -- Unsere neue Logic-Funktion aufrufen
                      runFileWithStart bottle path
                      return True -- Erfolg signalisieren
                  Nothing -> do
                      putStrLn "Error: Dropped item has no local path."
                      return False
          Nothing -> do
              putStrLn "Error: Dropped item is not a file."
              return False

  -- Controller zum Widget hinzufügen
  #addController dropZone dropTarget

  #append contentBox dropZone

  -- Trennlinie
  sep1 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 10, #marginBottom := 10 ]
  #append contentBox sep1

  -- === 2. Bereich: Installierte Programme (Dynamisch) ===
  
  -- Container für Expander und Refresh-Button nebeneinander
  progSectionBox <- new Gtk.Box 
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 10 
    ]
  #append contentBox progSectionBox

  -- Der Expander nimmt den meisten Platz ein (hexpand)
  progExpander <- new Gtk.Expander [ #label := tr "Installed Programs", #hexpand := True ]
  #append progSectionBox progExpander

  -- Box im Expander für die Programmliste
  progBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #marginTop := 10 ]
  #setChild progExpander (Just progBox)

  -- Hilfsfunktion zum Leeren der Box
  let clearBox box = do
        mChild <- Gtk.widgetGetFirstChild box
        case mChild of
          Just child -> do
            Gtk.boxRemove box child
            clearBox box
          Nothing -> return ()

  -- Die Logik zum Laden/Aktualisieren der Liste
  let refreshPrograms = do
        -- 1. Alte Einträge entfernen
        clearBox progBox
        
        -- 2. Neu scannen
        lnkFiles <- findWineStartMenuLnks bottle
        
        -- 3. Befüllen
        if null lnkFiles
          then do
            emptyLabel <- new Gtk.Label [ #label := tr "No programs found", #cssClasses := ["dim-label"] ]
            #append progBox emptyLabel
            -- Optional: Zuklappen wenn leer?
            -- set progExpander [ #expanded := False ]
          else do
            forM_ lnkFiles $ \path -> do
                let name = T.pack $ takeBaseName path
                progBtn <- new Gtk.Button 
                    [ #label := name
                    , #halign := Gtk.AlignFill 
                    , #tooltipText := T.pack path
                    ]
                on progBtn #clicked $ runWindowsLnk bottle path
                #append progBox progBtn
            -- Automatisch aufklappen, wenn Programme gefunden wurden
            set progExpander [ #expanded := True ]

  -- Der Refresh-Button (rechts vom Expander, oben ausgerichtet)
  refreshBtn <- new Gtk.Button 
    [ #iconName := "view-refresh-symbolic"
    , #tooltipText := tr "Refresh program list"
    , #valign := Gtk.AlignStart -- Wichtig: Damit er auf Höhe des Expander-Headers bleibt
    ]
  on refreshBtn #clicked refreshPrograms
  #append progSectionBox refreshBtn

  -- Initiales Laden der Programme beim Aufbau der View
  refreshPrograms

  -- Trennlinie
  sep2 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 10, #marginBottom := 10 ]
  #append contentBox sep2

  -- === 3. Bereich: System Tools ===
  toolsLabel <- new Gtk.Label [ #label := tr "System Tools", #halign := Gtk.AlignStart, #cssClasses := ["heading"] ]
  #append contentBox toolsLabel

  addBtn (tr "Wine Config") (tr "Opens winecfg") [] (runWineCfg bottle)
  addBtn (tr "Registry Editor") (tr "Opens regedit") [] (runRegedit bottle)
  addBtn (tr "Uninstaller") (tr "Manage installed programs") [] (runUninstaller bottle)

  hasWinetricks <- isWinetricksAvailable
  when hasWinetricks $ void $
    addBtn (tr "Winetricks") (tr "Manage packages and settings") [] (runWinetricks bottle)

  addBtn (tr "Browse Files") (tr "Open drive_c in file manager") [] (runFileManager bottle)
  
  -- Stop Button
  addBtn (tr "Stop all Programs") (tr "Forcefully close all running processes (wineserver -k)") ["destructive-action"] $ do
    showKillConfirmationDialog window bottle

  sep3 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 20, #marginBottom := 10 ]
  #append contentBox sep3

  addBtn (tr "Delete Bottle") (tr "Permanently delete this bottle") ["destructive-action"] $ do
    showDeleteConfirmationDialog window stack bottle refreshCallback

  backBtn <- new Gtk.Button [ #label := tr "Back to Library", #marginTop := 10 ]
  on backBtn #clicked $ #setVisibleChildName stack "overview"
  #append contentBox backBtn

  -- === LAYOUT STRUKTUR (Clamp & Scroll) ===
  
  clamp <- new Adw.Clamp 
    [ #child := contentBox
    , #maximumSize := 450
    , #tighteningThreshold := 300
    ]

  scrolledWindow <- new Gtk.ScrolledWindow 
    [ #child := clamp
    , #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #vscrollbarPolicy := Gtk.PolicyTypeAutomatic
    , #vexpand := True
    ]

  Gtk.toWidget scrolledWindow

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
