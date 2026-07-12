{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.NewBottleDialog where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
import Control.Monad (void)
import qualified Data.Text as T
import System.FilePath (takeBaseName)

import Bottle.Types
import Bottle.Logic
  ( checkNameValidity
  , NameValid(Valid)
  , explainNameValid
  , createBottleObject
  , createBottleLogic
  )
import Bottle.Logic.Runner (getAvailableRunners, getRunnerTypeDisplayName)
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

-- | Dialog for creating a new bottle.
-- Kept in Main for now, since it's triggered by the HeaderBar.
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
  
  nameEntry <- new Adw.EntryRow [ #title := tr "Name" ]
  #add group nameEntry

  runnerRow <- new Adw.ComboRow [ #title := tr "Runner" ]
  availableRunners <- getAvailableRunners

  runnerStrings <- mapM getRunnerTypeDisplayName availableRunners
  runnerModel <- Gtk.stringListNew (Just runnerStrings)

  #setModel runnerRow (Just runnerModel)
  #add group runnerRow

  #append contentBox group

  errorLabel <- new Gtk.Label
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #vexpand := False
    , #visible := False
    , #cssClasses := [T.pack "error"]
    , #marginStart := 20 
    , #marginEnd := 20
    , #marginBottom := 10
    ]
  #append contentBox errorLabel
  
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  void $ on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]
  
  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  validateName nameEntry createBtn errorLabel

  void $ on nameEntry #changed $
    validateName nameEntry createBtn errorLabel

  void $ on createBtn #clicked $ do
    nameText <- #getText nameEntry
    
    #setSensitive createBtn False
    #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
    #setVisible statusLabel True
    
    selectedRunnerIdx <- #getSelected runnerRow
    let selectedRunner = if fromIntegral selectedRunnerIdx < length availableRunners
                         then availableRunners !! fromIntegral selectedRunnerIdx
                         else SystemWine

    void $ async $ do
      bottleObj <- createBottleObject nameText selectedRunner
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

  #append btnBox cancelBtn
  #append btnBox createBtn
  
  #append contentBox statusLabel
  #append contentBox btnBox
  
  #setChild dialog (Just contentBox)
  #present dialog
