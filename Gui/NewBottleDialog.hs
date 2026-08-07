{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.NewBottleDialog where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
import Control.Monad (forM_, void)
import qualified Data.Text as T

import Bottle.Types
import Bottle.Logic (createBottleLogic)
import Bottle.Logic.Name (parseName, explainInvalidName)
import Bottle.Logic.Runner (getAvailableRunners, getRunnerTypeDisplayName)
import Gui.BottleView (runnerTypeToString)
import Logic.Translation (tr)

-- | Validates the name and updates the UI status accordingly.
validateName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow

  case parseName nameText of
    Right _ -> do
      #setSensitive createBtn True
      Gtk.widgetRemoveCssClass entryRow (T.pack "error")
      #setVisible errorLabel False
    Left reason -> do
      #setSensitive createBtn False
      Gtk.widgetAddCssClass entryRow (T.pack "error")
      #setLabel errorLabel (explainInvalidName reason)
      #setVisible errorLabel True

-- | Builds the "New Bottle" popover, meant to be attached to a
-- 'Gtk.MenuButton' via 'Gtk.setMenuButtonPopover' (see
-- 'Gui.OverviewView.buildOverviewPage'). Built once per view, like
-- 'Gui.BottleView.buildRunnerPopover' -- its form state (name entry, runner
-- selection, error/status labels) is reset on the popover's "closed" signal,
-- which fires uniformly whether it was dismissed by an outside click,
-- Escape, or an explicit 'Gtk.popoverPopdown' after a successful create.
--
-- The runner picker is a 'Gtk.ListBox' in 'Gtk.SelectionModeSingle' holding
-- one 'Adw.ActionRow' per runner, rather than an 'Adw.ComboRow' -- an
-- 'Adw.ComboRow's internal dropdown fights a surrounding 'Gtk.Popover' for
-- the pointer grab, so only its first entry could ever be selected (see
-- 'Gui.BottleView.buildRunnerPopover'). Using a plain 'Gtk.ListBox' instead
-- of a flat list of individually clickable rows lets GTK itself track and
-- render the current selection ('Gtk.listBoxGetSelectedRow'/'#selectRow'),
-- instead of hand-rolled 'Data.IORef's mirroring that state.
buildNewBottlePopover :: IO () -> IO Gtk.Popover
buildNewBottlePopover refreshCallback = do
  popover <- new Gtk.Popover []

  contentBox <- new Gtk.Box
    [ #orientation := Gtk.OrientationVertical
    , #spacing := 12
    , #marginTop := 12, #marginBottom := 12, #marginStart := 12, #marginEnd := 12
    , #widthRequest := 320
    ]
  #setChild popover (Just contentBox)

  nameGroup <- new Adw.PreferencesGroup []
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add nameGroup nameEntry
  #append contentBox nameGroup

  errorLabel <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #vexpand := False
    , #visible := False
    , #cssClasses := [T.pack "error"]
    ]
  #append contentBox errorLabel

  runnerLabel <- new Gtk.Label
    [ #label := tr "Runner"
    , #halign := Gtk.AlignStart
    , #cssClasses := ["heading"]
    ]
  #append contentBox runnerLabel

  runnerListBox <- new Gtk.ListBox
    [ #selectionMode := Gtk.SelectionModeSingle
    , #cssClasses := ["boxed-list"]
    ]
  #append contentBox runnerListBox

  availableRunners <- getAvailableRunners

  forM_ availableRunners $ \runnerType -> do
    displayName <- getRunnerTypeDisplayName (Existing runnerType)
    row <- new Adw.ActionRow
      [ #title := displayName
      , #subtitle := runnerTypeToString (Existing runnerType)
      ]
    #append runnerListBox row

  -- Default to the first runner, mirroring the previous ComboRow's default
  -- (index 0).
  defaultRow <- #getRowAtIndex runnerListBox 0
  #selectRow runnerListBox defaultRow

  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]

  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]

  validateName nameEntry createBtn errorLabel

  void $ on nameEntry #changed $
    validateName nameEntry createBtn errorLabel

  void $ on createBtn #clicked $ do
    nameText <- #getText nameEntry

    -- Re-runs the rules on the text as it is now, rather than trusting
    -- that 'validateName' left the button sensitive: the 'ValidName' it
    -- yields is what 'createBottleLogic' needs, so the check and the
    -- permission to create are the same step.
    case parseName nameText of
      Left reason -> do
        #setLabel statusLabel (explainInvalidName reason)
        #setVisible statusLabel True
      Right validName -> do
        #setSensitive createBtn False
        #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
        #setVisible statusLabel True

        mSelectedRow <- #getSelectedRow runnerListBox
        selectedRunner <- case mSelectedRow of
          Nothing -> pure SystemWine
          Just row -> do
            idx <- #getIndex row
            pure (availableRunners !! fromIntegral idx)

        void $ async $ do
          res <- try (createBottleLogic validName selectedRunner) :: IO (Either IOError ())

          GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
             case res of
               Right _ -> do
                 #popdown popover
                 refreshCallback
               Left err -> do
                 #setLabel statusLabel (T.pack $ "Error: " ++ show err)
                 #setSensitive createBtn True
             return False

  #append btnBox createBtn

  #append contentBox statusLabel
  #append contentBox btnBox

  -- Reset the form whenever the popover is dismissed, so the next time it
  -- opens it starts from a clean slate instead of showing the previous
  -- attempt's leftover name, error, or status text.
  void $ on popover #closed $ do
    #setText nameEntry ""
    #setVisible statusLabel False
    #setLabel statusLabel ""
    #selectRow runnerListBox defaultRow
    validateName nameEntry createBtn errorLabel

  pure popover
