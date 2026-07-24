{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, MonoLocalBinds #-}

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
  ( deleteBottleLogic
  , changeBottleRunnerLogic
  , isEngineFamilyChange
  , blockReason
  , explainBlockReason
  )
import Bottle.Logic.Process (killBottleProcesses)
import Bottle.Logic.Runner (getAvailableRunners, getRunnerTypeDisplayName)
import Bottle.Logic.Programs
  ( runExecutable
  , runFileWithStart
  , runWineCfg
  , runRegedit
  , runUninstaller
  , isWinetricksAvailable
  , runWinetricks
  , runFileManager
  )
import Bottle.Logic.Direct3dWrappers
  ( Direct3DWrapperState(..)
  , Direct3DWrapperStatus(..)
  , WrapperHealth(..)
  , getDirect3DWrapperState
  , getDirect3DWrapperStatus
  , setDirect3DWrapperState
  , repairDirect3DWrapperState
  )
import Bottle.Logic.Snapshots (isSnapshotableBottle)
import Logic.Translation (tr)
import Gui.BottleSnapshotsView (buildSnapshotView)
import Gui.ProgramListView (buildProgramListSection)

-- | Shows the confirmation dialog for deleting a bottle
showDeleteConfirmationDialog :: Gtk.Window -> Gtk.Stack -> Bottle -> (T.Text -> IO ()) -> IO () -> IO ()
showDeleteConfirmationDialog parent windowStack bottle showError refreshCallback = do
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
                      Left err -> showError $ tr "Failed to delete bottle: " <> T.pack (show err)
                    return False
  
  -- Explicit type annotation to disambiguate Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleAlertDialogResult)

-- | Shows the confirmation dialog for stopping all programs
showKillConfirmationDialog :: Gtk.Window -> Bottle -> (T.Text -> IO ()) -> IO ()
showKillConfirmationDialog parent bottle showError = do
  let message = tr "Stop all programs in this bottle?"
  let detail  = tr "This will execute 'wineserver -k' and force all running applications to close. Unsaved data may be lost."
  dialog <- new Gtk.AlertDialog [ #message := message, #detail  := detail, #buttons := [ tr "Cancel", tr "Stop All" ] ]
  let handleResult _ result = do
          buttonIndex <- Gtk.alertDialogChooseFinish dialog result
          when (buttonIndex == 1) $ do
              res <- try (killBottleProcesses bottle) :: IO (Either SomeException ())
              case res of
                  Left err -> showError $ tr "Failed to stop programs: " <> T.pack (show err)
                  Right _  -> putStrLn "Processes killed."
  
  -- Explicit type annotation to disambiguate Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleResult)

-- | Shows the confirmation dialog before switching between the Wine and
-- Proton engine families (see 'Bottle.Logic.isEngineFamilyChange') -- not
-- shown when switching between two builds of the same family, since that
-- doesn't mix two different engines' setup on the same prefix.
showRunnerChangeConfirmationDialog :: Gtk.Window -> Bottle -> RunnerType -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> IO ()
showRunnerChangeConfirmationDialog parent bottle newRunner stack showError refreshCallback = do
  let message = tr "Switch Windows Compatibility Layer?"
  let detail = tr "This bottle was set up under its current engine. Switching to a different one runs it through a different Windows compatibility layer on the same, already-initialized prefix -- the result is a mix of both engines' registry entries, DLL overrides, and prefix setup, which is hard to reproduce or diagnose afterwards.\n\nFor a clean result, create a new bottle for the other engine instead."
  dialog <- new Gtk.AlertDialog
    [ #message := message
    , #detail := detail
    , #buttons := [ tr "Cancel", tr "Switch Anyway" ]
    ]
  let handleResult _dialog result = do
        buttonIndex <- Gtk.alertDialogChooseFinish dialog result
        when (buttonIndex == 1) $ do
          updatedBottle <- changeBottleRunnerLogic bottle newRunner
          reloadBottleView parent updatedBottle stack showError refreshCallback

  -- Explicit type annotation to disambiguate Nothing
  Gtk.alertDialogChoose dialog (Just parent) (Nothing :: Maybe Gio.Cancellable) (Just handleResult)

-- | Builds the content of the runner-selection popover: one row per
-- available runner, with a checkmark on the currently active one. Clicking
-- a row changes the runner, reloads the bottle view, and closes the
-- popover again -- a popover also already closes itself on a click outside
-- it or Escape, so (unlike the previous Adw.MessageDialog) there is no
-- explicit Cancel button anymore. If the click crosses the Wine/Proton
-- engine boundary ('Bottle.Logic.isEngineFamilyChange'), the runner change
-- goes through 'showRunnerChangeConfirmationDialog' first instead of
-- applying immediately.
buildRunnerPopover :: Gtk.Window -> Bottle -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> [RunnerType] -> IO Gtk.Popover
buildRunnerPopover window bottle stack showError refreshCallback availableRunners = do
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
      if isEngineFamilyChange currentRunner runnerType
        then showRunnerChangeConfirmationDialog window bottle runnerType stack showError refreshCallback
        else do
          updatedBottle <- changeBottleRunnerLogic bottle runnerType
          reloadBottleView window updatedBottle stack showError refreshCallback

    #add runnerGroup row

  pure popover

-- | Builds the "Change Windows Runner" menu button: on click, opens a
-- popover directly below itself (instead of, as before, opening a separate
-- dialog window), see 'buildRunnerPopover'. Without any available runners
-- the button stays disabled, with an explanatory tooltip, instead of (as
-- before) only showing an error dialog after a click.
buildChangeRunnerButton :: Gtk.Window -> Bottle -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> IO Gtk.MenuButton
buildChangeRunnerButton window bottle stack showError refreshCallback = do
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
    popover <- buildRunnerPopover window bottle stack showError refreshCallback availableRunners
    #setPopover btn (Just popover)

  pure btn

-- | Adds a widget to a 'Gtk.Stack' under the given name, first removing
-- whatever child is already registered under that same name, if any.
-- 'Gtk.Stack' doesn't enforce unique names on its own -- 'addNamed'ing a
-- second child under a name already in use leaves both in the stack rather
-- than replacing the first, and 'setVisibleChildName' isn't guaranteed to
-- then resolve to the newly added one. Any call site that may re-add a page
-- under a name it could have already added before (e.g. re-entering the
-- same bottle's detail view, or reopening its snapshots view) needs this
-- instead of a bare 'addNamed', or a stale duplicate can silently linger
-- and resurface later.
replaceStackChild :: Gtk.Stack -> T.Text -> Gtk.Widget -> IO ()
replaceStackChild stack name widget = do
  mOldChild <- #getChildByName stack name
  forM_ mOldChild (#remove stack)
  void $ #addNamed stack widget (Just name)

-- | Reloads the bottle view
reloadBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> IO ()
reloadBottleView window bottle stack showError refreshCallback = do
  let viewName = "detail_" <> bottleName bottle
  newView <- buildBottleView window bottle stack showError refreshCallback
  replaceStackChild stack viewName newView
  #setVisibleChildName stack viewName

  refreshCallback

-- | Helper function to convert RunnerType to String
runnerTypeToString :: RunnerType -> T.Text
runnerTypeToString SystemWine = tr "System Wine"
runnerTypeToString (Proton path) = T.pack ("Proton (" ++ takeBaseName path ++ ")")
runnerTypeToString MissingSystemWine = tr "System Wine" <> " - " <> tr "not found"
runnerTypeToString (MissingProton path) = T.pack ("Proton (" ++ takeBaseName path ++ ")") <> " - " <> tr "not found"

-- | Display name and description for a Direct3D wrapper state.
direct3DWrapperLabel :: Direct3DWrapperState -> T.Text
direct3DWrapperLabel WineD3D             = tr "Wine (built-in)"
direct3DWrapperLabel Dxvk                = tr "DXVK"
direct3DWrapperLabel DxvkAndVkd3dProton  = tr "DXVK + vkd3d-proton"

-- | Unique name of a Direct3D wrapper state within the AdwToggleGroup;
-- losslessly convertible in both directions via 'Direct3DWrapperState's
-- derived 'Show'/'Read'.
direct3DWrapperName :: Direct3DWrapperState -> T.Text
direct3DWrapperName = T.pack . show

-- | Builds the Direct3D section of the bottle view. For "WrapperValid", an
-- AdwToggleGroup (a modern, segmented toggle widget) that switches between
-- Wine's built-in Direct3D implementation, DXVK, and DXVK + vkd3d-proton.
-- Symlinking the DLLs is a pure filesystem operation (no external
-- processes, no noticeable delay), so a synchronous call without a
-- progress indicator is enough -- like the other fast Logic calls in this
-- file (e.g. changeBottleRunnerLogic).
--
-- If the symlink is outdated or broken ("WrapperOutdated"/"WrapperDangling",
-- see Bottle.Logic.Direct3dWrappers.getDirect3DWrapperHealth), the
-- ToggleGroup is replaced by a single "Update" button that repairs it and
-- then reloads the whole view (so state and health are freshly determined
-- again and -- on success -- the ToggleGroup reappears).
buildDirect3DWrapperSection :: Gtk.Window -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> WrapperHealth -> Gtk.Box -> Bottle -> IO ()
buildDirect3DWrapperSection window stack showError refreshCallback health contentBox bottle = do
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
              Left err -> showError $ tr "Failed to change Direct3D wrapper: " <> T.pack (show err)
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
          Left err -> showError $ tr "Failed to repair Direct3D wrapper: " <> T.pack (show err)
          Right () -> reloadBottleView window bottle stack showError refreshCallback
      #append sectionBox updateBtn

-- | Creates the detail view for a bottle
buildBottleView :: Gtk.Window -> Bottle -> Gtk.Stack -> (T.Text -> IO ()) -> IO () -> IO Gtk.Widget
buildBottleView window bottle stack showError refreshCallback = do

  -- Adw.ToolbarView instead of Gtk.Box, for consistency with the other views
  toolbarView <- new Adw.ToolbarView []

  header <- new Adw.HeaderBar []

  backBtn <- new Gtk.Button [ #iconName := "go-previous-symbolic", #tooltipText := tr "Back to Library" ]
  let goBack = #setVisibleChildName stack "overview"
  void $ on backBtn #clicked goBack
  #packStart header backBtn

  winTitle <- new Adw.WindowTitle [ #title := bottleName bottle, #subtitle := tr "Bottle Details" ]
  #setTitleWidget header (Just winTitle)

  #addTopBar toolbarView header

  -- Escape or Alt+Left also navigates back, matching the GNOME/browser
  -- convention for "go back" -- attached to the view's own root widget so it
  -- only fires while this stack page actually holds keyboard focus.
  keyController <- new Gtk.EventControllerKey []
  void $ on keyController #keyPressed $ \keyval _keycode modifiers ->
    if keyval == Gdk.KEY_Escape || (keyval == Gdk.KEY_Left && Gdk.ModifierTypeAltMask `elem` modifiers)
      then goBack >> pure True
      else pure False
  #addController toolbarView keyController

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

  -- Status of the Direct3D wrapper -- controls whether/how the Direct3D
  -- section below is built. Whether Windows programs may be started at all
  -- is asked separately from the bottle (via 'blockReason', which also
  -- covers a missing runner, not just Direct3D-wrapper health), instead of
  -- deciding that here based on wrapper internals.
  direct3DStatus <- getDirect3DWrapperStatus bottle
  mBlockReason <- blockReason bottle
  let blockIfWineAppsBlocked :: Gtk.IsWidget w => w -> IO ()
      blockIfWineAppsBlocked widget = case mBlockReason of
        Nothing -> pure ()
        Just reason -> do
          w <- Gtk.toWidget widget
          set w [ #sensitive := False, #tooltipText := explainBlockReason reason ]

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

  runnerDisplayName <- getRunnerTypeDisplayName (runner bottle)
  runnerLabel <- new Gtk.Label 
    [ #label := runnerDisplayName
    , #halign := Gtk.AlignStart
    , #cssClasses := ["title-4"]
    ]
  #append runnerInfoBox runnerLabel

  runnerTypeLabel <- new Gtk.Label
    [ #label := runnerTypeToString (runner bottle)
    , #cssClasses := ["dim-label", "caption"]
    , #halign := Gtk.AlignStart
    ]
  #append runnerInfoBox runnerTypeLabel

  changeRunnerBtn <- buildChangeRunnerButton window bottle stack showError refreshCallback
  #append runnerSectionBox changeRunnerBtn

  -- Toggle the Direct3D wrapper (DXVK/vkd3d-proton) -- only if the bottle
  -- actually manages it itself per "direct3DStatus" (Proton already brings
  -- both along on its own).
  case direct3DStatus of
    WrapperManaged health -> buildDirect3DWrapperSection window stack showError refreshCallback health contentBox bottle
    WrapperNotManaged     -> pure ()

  let addBtn label tooltip cssClasses action = do
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip, #cssClasses := cssClasses, #halign := Gtk.AlignFill ]
        void $ on btn #clicked action
        #append contentBox btn
        return btn

  runBtn <- new Gtk.Button [ #label := tr "Run Executable / Installer", #cssClasses := ["suggested-action", "pill"], #halign := Gtk.AlignFill ]
  void $ on runBtn #clicked $ openExecutableFileDialog window showError $ runExecutable bottle
  #append contentBox runBtn
  blockIfWineAppsBlocked runBtn

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

  supportsSnapshots <- isSnapshotableBottle bottle
  when supportsSnapshots $ do
    snapBtn <- new Gtk.Button [ #cssClasses := ["pill"], #halign := Gtk.AlignFill, #marginBottom := 10 ]
    snapBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 8, #halign := Gtk.AlignCenter ]
    snapIcon <- new Gtk.Image [ #iconName := "camera-photo-symbolic" ]
    snapLabel <- new Gtk.Label [ #label := tr "Manage Snapshots" ]
    #append snapBox snapIcon >> #append snapBox snapLabel >> #setChild snapBtn (Just snapBox)
    
    void $ on snapBtn #clicked $ do
       snapView <- buildSnapshotView window bottle stack showError (reloadBottleView window bottle stack showError refreshCallback)
       let viewName = "snapshots_" <> bottleName bottle
       replaceStackChild stack viewName snapView
       #setVisibleChildName stack viewName
    #append contentBox snapBtn
    sepSnap <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginBottom := 10 ]
    #append contentBox sepSnap

  buildProgramListSection bottle (explainBlockReason <$> mBlockReason) contentBox

  sep2 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 10, #marginBottom := 10 ]
  #append contentBox sep2

  toolsLabel <- new Gtk.Label [ #label := tr "System Tools", #halign := Gtk.AlignStart, #cssClasses := ["heading"] ]
  #append contentBox toolsLabel
  addBtn (tr "Wine Config") (tr "Opens winecfg") [] (runWineCfg bottle) >>= blockIfWineAppsBlocked
  addBtn (tr "Registry Editor") (tr "Opens regedit") [] (runRegedit bottle) >>= blockIfWineAppsBlocked
  addBtn (tr "Uninstaller") (tr "Manage installed programs") [] (runUninstaller bottle) >>= blockIfWineAppsBlocked
  hasWinetricks <- isWinetricksAvailable
  when hasWinetricks $
    addBtn (tr "Winetricks") (tr "Manage packages") [] (runWinetricks bottle) >>= blockIfWineAppsBlocked
  -- Deliberately left unlocked: only opens the Linux file manager (xdg-open)
  -- on drive_c, so it doesn't start a Wine program.
  void $ addBtn (tr "Browse Files") (tr "Open drive_c") [] (runFileManager bottle)
  
  void $ addBtn (tr "Stop all Programs") (tr "Forcefully close all running processes") ["destructive-action"] $ showKillConfirmationDialog window bottle showError
  sep3 <- new Gtk.Separator [ #orientation := Gtk.OrientationHorizontal, #marginTop := 20, #marginBottom := 10 ]
  #append contentBox sep3
  void $ addBtn (tr "Delete Bottle") (tr "Permanently delete this bottle") ["destructive-action"] $ showDeleteConfirmationDialog window stack bottle showError refreshCallback

  Gtk.toWidget toolbarView

type FileSelectedCallback = FilePath -> IO ()

openExecutableFileDialog :: Gtk.Window -> (T.Text -> IO ()) -> FileSelectedCallback -> IO ()
openExecutableFileDialog parentWindow showError callback = do
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
    Gtk.fileDialogOpen dialog (Just parentWindow) (Just cancellable) (Just $ \_ result -> handleFileDialogResponse showError callback dialog result)

handleFileDialogResponse :: (T.Text -> IO ()) -> FileSelectedCallback -> Gtk.FileDialog -> Gio.AsyncResult -> IO ()
handleFileDialogResponse showError userCallback dialog result = do
    fileResult <- try (Gtk.fileDialogOpenFinish dialog result) :: IO (Either SomeException Gio.File)
    case fileResult of
        -- Cancelling the dialog also lands here (as a GError) -- not
        -- surfaced as a toast, since it's an expected, deliberate user
        -- action rather than a failure.
        Left err -> putStrLn $ "File dialog failed: " ++ show err
        Right gfile -> do
            mpath <- Gio.fileGetPath gfile
            case mpath of
                Just path -> userCallback path
                Nothing -> showError $ tr "The selected file is not a local file."
