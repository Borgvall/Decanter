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

-- | Überprüft, ob ein Bottle-Name gültig ist.
isNameValid :: T.Text -> Bool
isNameValid name
  | T.null name = False            -- Name ist leer
  | T.length name > 256 = False    -- Name ist zu lang
  | T.elem '/' name = False        -- Enthält einen Slash
  | otherwise = True               -- Name ist gültig

-- | Bestimmt die spezifische Fehlermeldung
determineError :: T.Text -> T.Text
determineError name
  | T.null name = tr "The name cannot be empty."
  | T.length name > 256 = tr "The name is too long (max 256 characters)."
  | T.elem '/' name = tr "The name cannot contain a slash ('/')."
  | otherwise = ""

-- | Die Logik zur Validierung des Namens und Aktualisierung des UI-Status.
validateName :: Adw.EntryRow -> Gtk.Button -> Gtk.Label -> IO ()
validateName entryRow createBtn errorLabel = do
  nameText <- #getText entryRow
  
  let valid = isNameValid nameText
  
  #setSensitive createBtn valid
  
  if valid
    then do
      -- Gültiger Name
      Gtk.widgetRemoveCssClass entryRow (T.pack "error") 
      #setVisible errorLabel False
    else do
      -- Ungültiger Name
      Gtk.widgetAddCssClass entryRow (T.pack "error")
      
      let errorMsg = determineError nameText
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
  
  model <- Gtk.stringListNew (Just $ map (T.pack . show) [Win64, Win32])
  
  #setModel archRow (Just model)
  #add group archRow
  
  #append contentBox group
  
  -- NEU: Das Label für die Fehlermeldung, platziert nach der PreferencesGroup
  errorLabel <- new Gtk.Label 
    [ #label := ""
    , #halign := Gtk.AlignStart
    , #vexpand := False
    , #visible := False
    -- Die Klasse 'error' sorgt dafür, dass das Label rot dargestellt wird, 
    -- vorausgesetzt das Theme unterstützt dies für Gtk.Label.
    , #cssClasses := [T.pack "error"]
    , #marginStart := 20 
    , #marginEnd := 20
    , #marginBottom := 10
    ]
  #append contentBox errorLabel
  
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]
  
  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  -- **NEUE LOGIK: On-the-fly Validierung**
  -- 1. Initialer Aufruf der Validierung
  validateName nameEntry createBtn errorLabel

  -- 2. Hinzufügen des Handlers für Textänderungen
  void $ on nameEntry #changed $
    validateName nameEntry createBtn errorLabel

  on createBtn #clicked $ do
    nameText <- #getText nameEntry
    
    -- **Hinzufügen eines letzten Checks, obwohl der Button deaktiviert sein sollte**
    if not (isNameValid nameText) 
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
