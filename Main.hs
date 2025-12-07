{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.Gio as Gio
import Data.GI.Base
import Control.Concurrent.Async (async)
import qualified Data.Text as T
import Control.Monad (void)

import Bottle.Types
import Bottle.Logic

main :: IO ()
main = do
  _ <- Gtk.init
  Adw.init
  app <- Adw.applicationNew (Just "com.haskell.bottles") []
  on app #activate (buildUI app)
  void $ #run app Nothing

buildUI :: Adw.Application -> IO ()
buildUI app = do
  window <- new Adw.ApplicationWindow [ #application := app, #title := "Haskell Bottles" ]
  stack <- new Gtk.Stack []
  
  (overview, _) <- buildOverviewPage stack
  #addNamed stack overview "overview"
  
  content <- new Adw.ToolbarView []
  #setContent content (Just stack)
  #setContent window (Just content)
  #present window

-- Seite 1: Übersicht
buildOverviewPage :: Gtk.Stack -> IO (Gtk.Widget, Bottle -> IO ())
buildOverviewPage stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 12, #marginTop := 20 ]
  
  statusLabel <- new Gtk.Label [ #label := tr "Welcome" ]
  
  createBtn <- new Gtk.Button 
    [ #label := tr "Create Demo Bottle" 
    , #tooltipText := tr "Creates a standard win64 prefix in ~/.local/share/bottles"
    ]
  
  on createBtn #clicked $ do
    Gtk.labelSetLabel statusLabel (tr "Creating...")
    async $ do
      let newBottle = Bottle "Demo" "/home/user/.local/share/bottles/Demo" SystemWine Win64
      res <- try (createBottleLogic newBottle) :: IO (Either IOError ())
      
      Gtk.idleAdd Gtk.PRIORITY_DEFAULT $ do
        case res of
          Right _ -> do
            detailView <- buildBottleView newBottle stack
            #addNamed stack detailView "detail_demo"
            #setVisibleChildName stack "detail_demo"
          Left err -> Gtk.labelSetLabel statusLabel (T.pack $ show err)
        return False
    return ()

  #append box statusLabel
  #append box createBtn
  return (toWidget box, \_ -> return ())

-- Seite 2: Detailansicht
buildBottleView :: Bottle -> Gtk.Stack -> IO Gtk.Widget
buildBottleView bottle stack = do
  box <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 10, #marginTop := 20 ]
  
  -- Titel
  title <- new Gtk.Label [ #label := bottleName bottle, #cssClasses := ["title-1"] ]
  #append box title

  -- Helper für Buttons mit Tooltips
  let addBtn label tooltip action = do
        btn <- new Gtk.Button [ #label := label, #tooltipText := tooltip ]
        on btn #clicked action
        #append box btn

  -- 1. Installer / Runner
  runBtn <- new Gtk.Button 
    [ #label := tr "Run Executable / Installer"
    , #tooltipText := tr "Select .exe or .msi file to run inside this bottle" 
    ]
  on runBtn #clicked $ do
    dialog <- Gtk.fileDialogNew
    filter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern filter "*.exe"
    Gtk.fileFilterAddPattern filter "*.msi"
    filters <- Gio.listStoreNew Gtk.FileFilter.gType
    Gio.listStoreAppend filters filter
    Gtk.fileDialogSetDefaultFilter dialog (Just filter)
    
    parent <- Gtk.widgetGetNative box
    Gtk.fileDialogOpen dialog (Just parent) Nothing $ \obj res -> do
      Gtk.fileDialogOpenFinish obj res >>= \case
        Just fileObj -> Gio.fileGetPath fileObj >>= \case
          Just p -> runExecutable bottle p
          Nothing -> return ()
        Nothing -> return ()
  #append box runBtn

  -- 2. Tools
  addBtn (tr "Wine Config") (tr "Opens winecfg") (runWineCfg bottle)
  addBtn (tr "Registry Editor") (tr "Opens regedit") (runRegedit bottle)
  addBtn (tr "Uninstaller") (tr "Manage installed programs") (runUninstaller bottle)
  addBtn (tr "Browse Files") (tr "Open drive_c in file manager") (runFileManager bottle)

  -- Zurück
  backBtn <- new Gtk.Button [ #label := tr "Back", #tooltipText := tr "Return to list" ]
  on backBtn #clicked $ #setVisibleChildName stack "overview"
  #append box backBtn

  return (toWidget box)
