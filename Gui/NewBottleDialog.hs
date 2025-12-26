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
  model <- Gtk.stringListNew (Just archStrings)
  
  #setModel archRow (Just model)
  #add group archRow
  
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
    
    selectedIdx <- #getSelected archRow
    
    -- NEU: Sicherer Zugriff auf die Architektur basierend auf dem Index
    -- Da die Liste supportedArchs genau der Reihenfolge im Model entspricht, passt der Index.
    let selectedArch = if fromIntegral selectedIdx < length supportedArchs
                       then supportedArchs !! fromIntegral selectedIdx
                       else Win64 -- Fallback (sollte nie eintreten)
    
    void $ async $ do
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

  #append btnBox cancelBtn
  #append btnBox createBtn
  
  #append contentBox statusLabel
  #append contentBox btnBox
  
  #setChild dialog (Just contentBox)
  #present dialog
