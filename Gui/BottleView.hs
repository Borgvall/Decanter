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
import Text.Read (readMaybe)

import Bottle.Types
import Bottle.Logic
import Bottle.Logic.Snapshots (isSnapshotableBottle)
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

-- | Baut den Inhalt des Runner-Auswahl-Popovers: eine Zeile pro verfügbarem
-- Runner, mit Checkmark beim aktuell aktiven. Ein Klick auf eine Zeile
-- ändert den Runner, lädt die Bottle-Ansicht neu und schließt das Popover
-- wieder -- ein Popover schließt sich zusätzlich schon von selbst bei Klick
-- daneben oder Escape, daher gibt es (anders als beim vorherigen
-- Adw.MessageDialog) keinen expliziten Cancel-Button mehr.
buildRunnerPopover :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> [RunnerType] -> IO Gtk.Popover
buildRunnerPopover window bottle stack refreshCallback availableRunners = do
  popover <- new Gtk.Popover []
  runnerGroup <- new Adw.PreferencesGroup
    [ #marginTop := 6, #marginBottom := 6, #marginStart := 6, #marginEnd := 6 ]
  #setChild popover (Just runnerGroup)

  let currentRunner = runner bottle
  forM_ availableRunners $ \runnerType -> do
    displayName <- getRunnerTypeDisplayName runnerType
    row <- new Adw.ActionRow
      [ #title := displayName
      , #subtitle := runnerTypeToString runnerType
      , #activatable := True
      ]

    when (runnerType == currentRunner) $ do
      icon <- new Gtk.Image [ #iconName := "object-select-symbolic", #cssClasses := ["dim-label"] ]
      #addSuffix row icon

    void $ on row #activated $ do
      #popdown popover
      updatedBottle <- changeBottleRunnerLogic bottle runnerType
      reloadBottleView window updatedBottle stack refreshCallback

    #add runnerGroup row

  pure popover

-- | Baut den "Change Windows Runner"-MenuButton: klappt bei Klick ein
-- Popover direkt unter sich selbst auf (statt, wie zuvor, ein separates
-- Dialogfenster zu öffnen), siehe 'buildRunnerPopover'. Ohne verfügbare
-- Runner bleibt der Button deaktiviert, mit erklärendem Tooltip, statt
-- (wie zuvor) erst nach einem Klick einen Fehlerdialog zu zeigen.
buildChangeRunnerButton :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO Gtk.MenuButton
buildChangeRunnerButton window bottle stack refreshCallback = do
  availableRunners <- getAvailableRunners
  let hasRunners = not (null availableRunners)

  content <- new Adw.ButtonContent
    [ #iconName := "edit-symbolic"
    , #label := tr "Change Windows Runner"
    ]
  btn <- new Gtk.MenuButton
    [ #child := content
    , #tooltipText := if hasRunners
        then tr "Change Runner"
        else tr "No runners available. Please install Wine or Proton."
    , #valign := Gtk.AlignCenter
    , #cssClasses := ["flat"]
    , #sensitive := hasRunners
    , #alwaysShowArrow := True
    ]

  when hasRunners $ do
    popover <- buildRunnerPopover window bottle stack refreshCallback availableRunners
    #setPopover btn (Just popover)

  pure btn

-- | Lädt die Bottle-Ansicht neu
reloadBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> IO () -> IO ()
reloadBottleView window bottle stack refreshCallback = do
  -- Aktuelle View aus dem Stack entfernen. Der Name muss mit dem
  -- übereinstimmen, unter dem Gui.OverviewView die Detailansicht ursprünglich
  -- via #addNamed hinzugefügt hat -- sonst wird die alte View nie entfernt
  -- und stattdessen läuft hier bei jedem Aufruf eine zusätzliche, unsichtbare
  -- Karteikarte im Stack auf.
  let viewName = "detail_" <> bottleName bottle
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

-- | Zur Auswahl angebotene Kategorien für Anwendungsmenü-Einträge (siehe
-- 'Bottle.Logic.addToApplicationMenu'). Dies sind exakte Freedesktop-
-- "Main Category"-Bezeichner (landen 1:1 im "Categories="-Feld der
-- ".desktop"-Datei) und werden daher bewusst nicht übersetzt. "Game" steht
-- an erster Stelle, da das der häufigste Anwendungsfall von Decanter ist.
applicationMenuCategories :: [T.Text]
applicationMenuCategories =
  [ "Game", "Utility", "Office", "Graphics", "Network", "Development", "AudioVideo", "Education", "System" ]

-- | Baut den Button, mit dem eine einzelne Start-Menü-Applikation zum
-- Anwendungsmenü des Hosts hinzugefügt bzw. wieder daraus entfernt werden
-- kann (siehe 'Bottle.Logic.addToApplicationMenu'/'removeFromApplicationMenu').
-- Der Zustand (schon hinzugefügt oder nicht) wird bei jedem Aufruf frisch
-- über 'isInApplicationMenu' geprüft; "onChanged" lässt den Aufrufer nach
-- einer Änderung die Programmliste neu aufbauen, damit der Button seinen
-- Zustand aktualisiert. Bleibt bewusst ungesperrt, auch wenn Windows-
-- Programme aktuell blockiert sind (siehe "Browse Files" weiter unten): es
-- wird nur eine ".desktop"-Datei geschrieben/gelöscht, kein Wine-Programm
-- selbst gestartet.
buildAppMenuButton :: Bottle -> T.Text -> FilePath -> IO () -> IO Gtk.Widget
buildAppMenuButton bottle appName lnkPath onChanged = do
  alreadyAdded <- isInApplicationMenu bottle appName
  if alreadyAdded
    then do
      btn <- new Gtk.Button
        [ #iconName := "list-remove-symbolic"
        , #tooltipText := tr "Remove from application menu"
        , #valign := Gtk.AlignCenter
        ]
      void $ on btn #clicked $ do
        removeFromApplicationMenu bottle appName
        onChanged
      Gtk.toWidget btn
    else do
      menuBtn <- new Gtk.MenuButton
        [ #iconName := "list-add-symbolic"
        , #tooltipText := tr "Add to application menu"
        , #valign := Gtk.AlignCenter
        ]

      popover <- new Gtk.Popover []
      popBox <- new Gtk.Box
        [ #orientation := Gtk.OrientationVertical
        , #spacing := 8
        , #marginTop := 8, #marginBottom := 8, #marginStart := 8, #marginEnd := 8
        ]
      #setChild popover (Just popBox)

      headingLabel <- new Gtk.Label
        [ #label := tr "Add to application menu"
        , #halign := Gtk.AlignStart
        , #cssClasses := ["heading"]
        ]
      #append popBox headingLabel

      -- Flache Liste anklickbarer Zeilen statt eines verschachtelten
      -- Dropdowns (z.B. Adw.ComboRow): ein GTK4-Popup innerhalb eines
      -- anderen Popups kollidiert beim Pointer-Grab, sodass sich nur der
      -- erste Eintrag auswählen ließ. Klick auf eine Zeile fügt sofort mit
      -- dieser Kategorie hinzu, genau wie beim Runner-Popover oben.
      categoryGroup <- new Adw.PreferencesGroup [ #title := tr "Category" ]
      #append popBox categoryGroup

      forM_ applicationMenuCategories $ \category -> do
        row <- new Adw.ActionRow [ #title := category, #activatable := True ]
        -- addToApplicationMenu extrahiert dabei per Wine/winemenubuilder ein
        -- Icon (siehe Bottle.Logic.Process.extractAppIcon) und kann daher
        -- ein paar Sekunden brauchen -- läuft deshalb (wie das
        -- Bottle-Löschen oben) in einem eigenen Thread, damit die UI nicht
        -- kurz einfriert.
        void $ on row #activated $ do
          #popdown popover
          void $ async $ do
            addToApplicationMenu bottle appName lnkPath category
            GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
              onChanged
              return False
        #add categoryGroup row

      #setPopover menuBtn (Just popover)
      Gtk.toWidget menuBtn

-- | Anzeigename und Beschreibung für einen Direct3D-Wrapper-Zustand.
direct3DWrapperLabel :: Direct3DWrapperState -> T.Text
direct3DWrapperLabel WineD3D             = tr "Wine (built-in)"
direct3DWrapperLabel Dxvk                = tr "DXVK"
direct3DWrapperLabel DxvkAndVkd3dProton  = tr "DXVK + vkd3d-proton"

-- | Eindeutiger Name eines Direct3D-Wrapper-Zustands innerhalb der
-- AdwToggleGroup; über 'Direct3DWrapperState's abgeleitetes 'Show'/'Read'
-- verlustfrei in beide Richtungen konvertierbar.
direct3DWrapperName :: Direct3DWrapperState -> T.Text
direct3DWrapperName = T.pack . show

-- | Baut die Direct3D-Sektion der Bottle-Ansicht. Bei "WrapperValid" eine
-- AdwToggleGroup (ein modernes, segmentiertes Umschalt-Widget), mit der
-- zwischen Wines eingebauter Direct3D-Implementierung, DXVK und DXVK +
-- vkd3d-proton umgeschaltet werden kann. Symlinken der DLLs ist eine reine
-- Dateisystem-Operation (keine externen Prozesse, keine spürbare
-- Verzögerung), daher genügt ein synchroner Aufruf ohne Fortschrittsanzeige
-- -- wie bei den übrigen schnellen Logic-Aufrufen in dieser Datei (z.B.
-- changeBottleRunnerLogic).
--
-- Ist der Symlink veraltet oder defekt ("WrapperOutdated"/"WrapperDangling",
-- siehe Bottle.Logic.Direct3dWrappers.getDirect3DWrapperHealth), wird die
-- ToggleGroup durch einen einzelnen "Update"-Button ersetzt, der repariert
-- und danach die ganze Ansicht neu lädt (damit Zustand und Health frisch neu
-- ermittelt werden und -- bei Erfolg -- wieder die ToggleGroup erscheint).
buildDirect3DWrapperSection :: Gtk.Window -> Gtk.Stack -> IO () -> WrapperHealth -> Gtk.Box -> Bottle -> IO ()
buildDirect3DWrapperSection window stack refreshCallback health contentBox bottle = do
  currentState <- getDirect3DWrapperState bottle

  sectionBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 4
    , #marginBottom := 15
    ]
  #append contentBox sectionBox

  sectionLabel <- new Gtk.Label
    [ #label := tr "Direct3D"
    , #halign := Gtk.AlignStart
    , #hexpand := True
    , #cssClasses := ["dim-label", "caption"]
    ]
  #append sectionBox sectionLabel

  case health of
    WrapperValid -> do
      let tooltip = tr "For current games, \"DXVK + vkd3d-proton\" is the recommended setting. Otherwise it may not matter, or results can vary."

      toggleGroup <- new Adw.ToggleGroup [ #halign := Gtk.AlignEnd ]
      #append sectionBox toggleGroup

      forM_ [WineD3D, Dxvk, DxvkAndVkd3dProton] $ \state -> do
        toggle <- new Adw.Toggle
          [ #name := direct3DWrapperName state
          , #label := direct3DWrapperLabel state
          ]
        Adw.toggleSetTooltip toggle tooltip
        Adw.toggleGroupAdd toggleGroup toggle
      Adw.toggleGroupSetActiveName toggleGroup (Just (direct3DWrapperName currentState))

      void $ on toggleGroup (PropertyNotify #activeName) $ \_pspec -> do
        maybeName <- Adw.toggleGroupGetActiveName toggleGroup
        case maybeName >>= readMaybe . T.unpack of
          Nothing    -> pure ()
          Just state -> do
            result <- try (setDirect3DWrapperState bottle state) :: IO (Either SomeException ())
            case result of
              Left err -> putStrLn $ "Error changing Direct3D wrapper: " ++ show err
              Right () -> pure ()

    _ -> do
      let updateTooltip = case health of
            WrapperDangling ->
              tr "The Direct3D wrapper's files are missing (e.g. removed by Nix garbage collection). Windows programs are blocked until this is repaired."
            _ -> tr "A newer Direct3D wrapper version is available."

      updateBtn <- new Gtk.Button
        [ #label := tr "Update"
        , #tooltipText := updateTooltip
        , #halign := Gtk.AlignEnd
        , #cssClasses := ["suggested-action"]
        ]
      void $ on updateBtn #clicked $ do
        result <- try (repairDirect3DWrapperState bottle) :: IO (Either SomeException ())
        case result of
          Left err -> putStrLn $ "Error repairing Direct3D wrapper: " ++ show err
          Right () -> reloadBottleView window bottle stack refreshCallback
      #append sectionBox updateBtn

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

  -- Status des Direct3D-Wrappers -- steuert, ob/wie die Direct3D-Sektion
  -- unten aufgebaut wird. Ob überhaupt Windows-Programme gestartet werden
  -- dürfen, fragen wir separat bei der Bottle nach ("readyForWindowsApps"),
  -- statt hier selbst über Wrapper-Interna zu entscheiden.
  direct3DStatus <- getDirect3DWrapperStatus bottle
  readyForWindowsApps <- isBottleReadyForWindowsApps bottle
  let blockedTooltip = tr "Blocked until the Direct3D wrapper is repaired (see above)."
      blockIfWineAppsBlocked :: Gtk.IsWidget w => w -> IO ()
      blockIfWineAppsBlocked widget = when (not readyForWindowsApps) $ do
        w <- Gtk.toWidget widget
        set w [ #sensitive := False, #tooltipText := blockedTooltip ]

  -- NEU: Runner-Information anzeigen mit Änderungs-Button
  runnerSectionBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationHorizontal
    , #spacing := 8
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
  changeRunnerBtn <- buildChangeRunnerButton window bottle stack refreshCallback
  #append runnerSectionBox changeRunnerBtn

  -- Direct3D-Wrapper (DXVK/vkd3d-proton) umschalten -- nur wenn die Bottle
  -- das laut "direct3DStatus" überhaupt selbst verwaltet (Proton bringt
  -- beides bereits selbst mit).
  case direct3DStatus of
    WrapperManaged health -> buildDirect3DWrapperSection window stack refreshCallback health contentBox bottle
    WrapperNotManaged     -> pure ()

  let addBtn label tooltip cssClasses action = do
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip, #cssClasses := cssClasses, #halign := Gtk.AlignFill ]
        void $ on btn #clicked action
        #append contentBox btn
        return btn

  -- Buttons & Content (Run, DropZone, etc. wie gehabt)
  runBtn <- new Gtk.Button [ #label := tr "Run Executable / Installer", #cssClasses := ["suggested-action", "pill"], #halign := Gtk.AlignFill ]
  void $ on runBtn #clicked $ openExecutableFileDialog window $ runExecutable bottle
  #append contentBox runBtn
  blockIfWineAppsBlocked runBtn

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
  blockIfWineAppsBlocked dropZone

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
       snapView <- buildSnapshotView window bottle stack (reloadBottleView window bottle stack refreshCallback)
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
                rowBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 4 ]

                progBtn <- new Gtk.Button [ #label := name, #hexpand := True, #halign := Gtk.AlignFill, #tooltipText := T.pack path ]
                void $ on progBtn #clicked $ runWindowsLnk bottle path
                #append rowBox progBtn
                blockIfWineAppsBlocked progBtn

                menuBtn <- buildAppMenuButton bottle name path refreshPrograms
                #append rowBox menuBtn

                #append progBox rowBox
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
  addBtn (tr "Wine Config") (tr "Opens winecfg") [] (runWineCfg bottle) >>= blockIfWineAppsBlocked
  addBtn (tr "Registry Editor") (tr "Opens regedit") [] (runRegedit bottle) >>= blockIfWineAppsBlocked
  addBtn (tr "Uninstaller") (tr "Manage installed programs") [] (runUninstaller bottle) >>= blockIfWineAppsBlocked
  hasWinetricks <- isWinetricksAvailable
  when hasWinetricks $
    addBtn (tr "Winetricks") (tr "Manage packages") [] (runWinetricks bottle) >>= blockIfWineAppsBlocked
  -- Bleibt bewusst nicht gesperrt: öffnet nur den Linux-Dateimanager (xdg-open)
  -- auf drive_c, startet also kein Wine-Programm.
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
