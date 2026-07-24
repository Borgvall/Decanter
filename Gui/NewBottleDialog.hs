{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.NewBottleDialog where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
import Control.Monad (forM_, void)
import Data.IORef
import qualified Data.Text as T

import Bottle.Types
import Bottle.Logic
  ( checkNameValidity
  , NameValid(Valid)
  , explainNameValid
  , createBottleObject
  , createBottleLogic
  )
import Bottle.Logic.Runner (getAvailableRunners, getRunnerTypeDisplayName)
import Gui.BottleView (runnerTypeToString)
import Logic.Translation (tr)

-- | Validates the name and updates the UI status accordingly.
validateName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow

  let status = checkNameValidity nameText
  let valid = status == Valid

  #setSensitive createBtn valid

  if valid
    then do
      Gtk.widgetRemoveCssClass entryRow (T.pack "error")
      #setVisible errorLabel False
    else do
      Gtk.widgetAddCssClass entryRow (T.pack "error")

      let errorMsg = explainNameValid status
      #setLabel errorLabel errorMsg
      #setVisible errorLabel True

-- | Builds the "New Bottle" popover, meant to be attached to a
-- 'Gtk.MenuButton' via 'Gtk.setMenuButtonPopover' (see
-- 'Gui.OverviewView.buildOverviewPage'). Built once per view, like
-- 'Gui.BottleView.buildRunnerPopover' -- its form state (name entry, runner
-- selection, error/status labels) is reset on the popover's "closed" signal,
-- which fires uniformly whether it was dismissed by an outside click,
-- Escape, or an explicit 'Gtk.popoverPopdown' after a successful create.
--
-- The runner picker is a flat list of clickable 'Adw.ActionRow's rather than
-- an 'Adw.ComboRow' -- an 'Adw.ComboRow's internal dropdown fights a
-- surrounding 'Gtk.Popover' for the pointer grab, so only its first entry
-- could ever be selected (see 'Gui.BottleView.buildRunnerPopover').
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

  runnerGroup <- new Adw.PreferencesGroup [ #title := tr "Runner" ]
  #append contentBox runnerGroup

  availableRunners <- getAvailableRunners
  let defaultRunner = case availableRunners of
        (r : _) -> r
        [] -> SystemWine
  selectedRunnerRef <- newIORef defaultRunner
  runnerIconsRef <- newIORef ([] :: [(RunnerType, Gtk.Image)])

  forM_ availableRunners $ \runnerType -> do
    displayName <- getRunnerTypeDisplayName runnerType
    row <- new Adw.ActionRow
      [ #title := displayName
      , #subtitle := runnerTypeToString runnerType
      , #activatable := True
      ]

    icon <- new Gtk.Image
      [ #iconName := "object-select-symbolic"
      , #cssClasses := ["dim-label"]
      , #visible := (runnerType == defaultRunner)
      ]
    #addSuffix row icon
    #add runnerGroup row

    modifyIORef' runnerIconsRef ((runnerType, icon) :)

    void $ on row #activated $ do
      writeIORef selectedRunnerRef runnerType
      icons <- readIORef runnerIconsRef
      forM_ icons $ \(rt, ic) -> #setVisible ic (rt == runnerType)

  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]

  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]

  validateName nameEntry createBtn errorLabel

  void $ on nameEntry #changed $
    validateName nameEntry createBtn errorLabel

  void $ on createBtn #clicked $ do
    nameText <- #getText nameEntry

    #setSensitive createBtn False
    #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
    #setVisible statusLabel True

    selectedRunner <- readIORef selectedRunnerRef

    void $ async $ do
      bottleObj <- createBottleObject nameText selectedRunner
      res <- try (createBottleLogic bottleObj) :: IO (Either IOError ())

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
    writeIORef selectedRunnerRef defaultRunner
    icons <- readIORef runnerIconsRef
    forM_ icons $ \(rt, ic) -> #setVisible ic (rt == defaultRunner)
    validateName nameEntry createBtn errorLabel

  pure popover
