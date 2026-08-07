{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.ProgramListView (buildProgramListSection) where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import qualified Data.Text as T
import Control.Monad (forM_, void)
import System.FilePath (takeBaseName)

import Bottle.Types
import Bottle.Logic.ApplicationMenu
  ( addToApplicationMenu
  , removeFromApplicationMenu
  , isInApplicationMenu
  )
import Bottle.Logic.Programs (findWineStartMenuLnks, runWindowsLnk)
import Logic.Translation (tr)

-- | Categories offered for application-menu entries (see
-- 'Bottle.Logic.addToApplicationMenu'). These are exact Freedesktop
-- "Main Category" identifiers (they end up verbatim in the ".desktop"
-- file's "Categories=" field), so they are deliberately not translated.
-- "Game" comes first since that's Decanter's most common use case.
applicationMenuCategories :: [T.Text]
applicationMenuCategories =
  [ "Game", "Utility", "Office", "Graphics", "Network", "Development", "AudioVideo", "Education", "System" ]

-- | Builds the button used to add or remove a single start-menu
-- application to/from the host's application menu (see
-- 'Bottle.Logic.addToApplicationMenu'/'removeFromApplicationMenu'). The
-- state (already added or not) is freshly checked via 'isInApplicationMenu'
-- on every call; "onChanged" lets the caller rebuild the program list after
-- a change, so the button's state updates. Deliberately left unlocked even
-- while Windows programs are currently blocked (see
-- 'buildProgramListSection'): this only writes/deletes a ".desktop" file,
-- it doesn't start a Wine program itself.
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

      -- A flat list of clickable rows instead of a nested dropdown (e.g.
      -- Adw.ComboRow): a GTK4 popup inside another popup collides during
      -- the pointer grab, so only the first entry could ever be selected.
      -- Clicking a row adds immediately with that category, just like the
      -- runner popover in Gui.BottleView.
      categoryGroup <- new Adw.PreferencesGroup [ #title := tr "Category" ]
      #append popBox categoryGroup

      forM_ applicationMenuCategories $ \category -> do
        row <- new Adw.ActionRow [ #title := category, #activatable := True ]
        -- addToApplicationMenu extracts an icon via Wine/winemenubuilder
        -- (see Bottle.Logic.Process.extractAppIcon), so it can take a few
        -- seconds -- runs in its own thread so this doesn't briefly freeze
        -- the UI.
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


-- | Builds the "Installed Programs" section: an expander listing every
-- Wine start-menu shortcut found in the bottle, each with a button to run
-- it and a button to add/remove it from the host's application menu (see
-- 'buildAppMenuButton'). Appends itself directly into "contentBox", like
-- 'Gui.BottleView.buildDirect3DWrapperSection' does.
--
-- "launchable" is what 'Gui.BottleView.buildBottleView' got from
-- 'Bottle.Logic.launchableRunner' and also drives its own buttons with:
-- 'Right' carries the runner to launch programs with, 'Left' a tooltip
-- explaining why nothing can be launched. The blocked side is passed down
-- as already-rendered text rather than a 'BlockReason' so this module
-- doesn't need to know about Direct3D/runner internals itself.
--
-- The application-menu button is deliberately not blocked by it, see
-- 'buildAppMenuButton'.
buildProgramListSection :: Bottle -> Either T.Text ExistingRunner -> Gtk.Box -> IO ()
buildProgramListSection bottle launchable contentBox = do
  progSectionBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10 ]
  #append contentBox progSectionBox
  progExpander <- new Gtk.Expander [ #label := tr "Installed Programs", #hexpand := True ]
  #append progSectionBox progExpander
  progBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #marginTop := 10 ]
  #setChild progExpander (Just progBox)

  let blockIfBlocked :: Gtk.Button -> IO ()
      blockIfBlocked btn = case launchable of
        Right _      -> pure ()
        Left tooltip -> set btn [ #sensitive := False, #tooltipText := tooltip ]

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
                -- With nothing to launch with, the button gets no handler at
                -- all; 'blockIfBlocked' disables it in that case anyway.
                case launchable of
                  Right r -> void $ on progBtn #clicked $ runWindowsLnk bottle r path
                  Left _  -> pure ()
                #append rowBox progBtn
                blockIfBlocked progBtn

                menuBtn <- buildAppMenuButton bottle name path refreshPrograms
                #append rowBox menuBtn

                #append progBox rowBox
            set progExpander [ #expanded := True ]

  refreshBtn <- new Gtk.Button [ #iconName := "view-refresh-symbolic", #tooltipText := tr "Refresh program list", #valign := Gtk.AlignStart ]
  void $ on refreshBtn #clicked refreshPrograms
  #append progSectionBox refreshBtn
  refreshPrograms
