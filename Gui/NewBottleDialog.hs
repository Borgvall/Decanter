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
import Logic.Translation (tr)

-- | Die Logik zur Validierung des Namens und Aktualisierung des UI-Status.
validateName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow
  
  let status = checkNameValidity nameText
  let valid = status == Valid
  
  #setSensitive createBtn valid
  
  if valid
    then do
      -- Gültiger Name
      Gtk.widgetRemoveCssClass entryRow (T.pack "error") 
      #setVisible errorLabel False
    else do
      -- Ungültiger Name
      Gtk.widgetAddCssClass entryRow (T.pack "error")
      
      let errorMsg = explainNameValid status
      #setLabel errorLabel errorMsg
      #setVisible errorLabel True

-- | Dialog zum Erstellen einer neuen Bottle
-- Diesen lassen wir vorerst in Main, da er von der HeaderBar getriggert wird.
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
  
  archRow <- new Adw.ComboRow [ #title := tr "Architecture" ]
  
  -- NEU: Unterstützte Architekturen dynamisch abrufen
  supportedArchs <- getSupportedArchitectures
  
  -- Konvertiere die Architekturen in Strings für die Anzeige (nutze show für "Win64"/"Win32")
  let archStrings = map (T.pack . show) supportedArchs
  archModel <- Gtk.stringListNew (Just archStrings)
  
  #setModel archRow (Just archModel)
  #add group archRow

  -- NEU: Runner Auswahl
  runnerRow <- new Adw.ComboRow [ #title := tr "Runner" ]
  availableRunners <- getAvailableRunners

  let formatRunner r = case r of
        SystemWine -> "System Wine"
        Proton p   -> "Proton " <> T.pack (takeBaseName p)

  let runnerStrings = map formatRunner availableRunners
  runnerModel <- Gtk.stringListNew (Just runnerStrings)
  #setModel runnerRow (Just runnerModel)
  #add group runnerRow
  
  #append contentBox group
  
  -- Das Label für die Fehlermeldung
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

  -- Validierung initialisieren
  validateName nameEntry createBtn errorLabel

  void $ on nameEntry #changed $
    validateName nameEntry createBtn errorLabel

  void $ on createBtn #clicked $ do
    nameText <- #getText nameEntry
    
    #setSensitive createBtn False
    #setLabel statusLabel (tr "Creating prefix (this may take a while)...")
    #setVisible statusLabel True
    
    -- Architektur bestimmen
    selectedArchIdx <- #getSelected archRow
    let selectedArch = if fromIntegral selectedArchIdx < length supportedArchs
                       then supportedArchs !! fromIntegral selectedArchIdx
                       else Win64 
    
    -- Runner bestimmen
    selectedRunnerIdx <- #getSelected runnerRow
    let selectedRunner = if fromIntegral selectedRunnerIdx < length availableRunners
                         then availableRunners !! fromIntegral selectedRunnerIdx
                         else SystemWine

    void $ async $ do
      -- Wir übergeben jetzt auch den ausgewählten Runner
      bottleObj <- createBottleObject nameText selectedArch selectedRunner
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
