{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.NewBottleDialog where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import Control.Exception (try)
import qualified Data.Text as T

import Bottle.Types
import Bottle.Logic
import Logic.Translation (tr)

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
  
  btnBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10, #halign := Gtk.AlignEnd ]
  
  cancelBtn <- new Gtk.Button [ #label := tr "Cancel" ]
  on cancelBtn #clicked $ #close dialog
  
  createBtn <- new Gtk.Button [ #label := tr "Create", #cssClasses := ["suggested-action"] ]
  
  statusLabel <- new Gtk.Label [ #label := "", #visible := False ]

  on createBtn #clicked $ do
    nameText <- #getText nameEntry
    
    if T.null nameText 
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
