{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gui.ProgramListView (buildProgramListSection) where

import qualified GI.Gtk as Gtk
import qualified GI.Adw as Adw
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Concurrent.Async (async)
import qualified Data.Text as T
import Control.Monad (forM, forM_, unless, void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)

import Bottle.Types
import Bottle.Logic.ApplicationMenu
  ( addToApplicationMenu
  , removeFromApplicationMenu
  , isInApplicationMenu
  , applicationIconPath
  )
import Bottle.Logic.Process (extractAppIcons)
import Bottle.Logic.Programs (findWineStartMenuLnks, runWindowsLnk)
import Logic.Translation (tr)

-- | Icon shown for a program whose real icon hasn't been extracted yet (or
-- can't be: a bottle whose runner is missing can't run Wine to extract it).
missingIconName :: T.Text
missingIconName = "application-x-executable-symbolic"

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
-- a change, so the button's state updates.
--
-- The two directions are gated differently on purpose. *Adding* needs a
-- launchable bottle: it extracts the entry's icon by running Wine/Proton
-- (see 'Bottle.Logic.Process.extractAppIcons'), and the entry it writes has
-- an "Exec=decanter start ...", which would fail the moment anyone clicked
-- it in the host's menu. *Removing* only deletes a ".desktop" file and
-- stays available precisely then -- otherwise a bottle whose runner
-- disappeared would keep a dead menu entry that can no longer be cleaned
-- up from here.
buildAppMenuButton :: Bottle -> Either T.Text (BottleG ExistingRunner) -> T.Text -> FilePath -> IO () -> IO Gtk.Widget
buildAppMenuButton bottle launchable appName lnkPath onChanged = do
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
    else case launchable of
      Left blockTooltip -> do
        blockedBtn <- new Gtk.MenuButton
          [ #iconName := "list-add-symbolic"
          , #tooltipText := blockTooltip
          , #valign := Gtk.AlignCenter
          , #sensitive := False
          ]
        Gtk.toWidget blockedBtn

      Right runnable -> do
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
          -- (see Bottle.Logic.Process.extractAppIcons), so it can take a few
          -- seconds -- runs in its own thread so this doesn't briefly freeze
          -- the UI.
          void $ on row #activated $ do
            #popdown popover
            void $ async $ do
              addToApplicationMenu runnable appName lnkPath category
              GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                onChanged
                return False
          #add categoryGroup row

        #setPopover menuBtn (Just popover)
        Gtk.toWidget menuBtn

-- | Builds the "Installed Programs" section: an expander listing every
-- Wine start-menu shortcut found in the bottle, each with its icon, a button
-- to run it and a button to add/remove it from the host's application menu
-- (see 'buildAppMenuButton'). Appends itself directly into "contentBox",
-- like 'Gui.BottleView.buildDirect3DWrapperSection' does.
--
-- "launchable" is what 'Gui.BottleView.buildBottleView' got from
-- 'Bottle.Logic.launchableBottle' and also drives its own buttons with:
-- 'Right' carries the bottle at a type that says its runner is installed,
-- 'Left' a tooltip explaining why nothing can be launched. The blocked side
-- is passed down as already-rendered text rather than a 'BlockReason' so
-- this module doesn't need to know about Direct3D/runner internals itself.
--
-- Icons come from the same cache the application-menu entries use (see
-- 'applicationIconPath'): icons already extracted are shown immediately,
-- the rest are extracted in the background by one batched Wine/Proton
-- invocation and filled in as they arrive. That keeps the list usable while
-- extraction runs - it takes a few seconds even batched, and it needs a
-- launchable bottle at all (a missing runner can't run Wine, so those rows
-- keep the placeholder icon).
--
-- See 'buildAppMenuButton' for why the application-menu button is only
-- half-blocked by this (removing stays possible, adding doesn't).
buildProgramListSection :: Bottle -> Either T.Text (BottleG ExistingRunner) -> Gtk.Box -> IO ()
buildProgramListSection bottle launchable contentBox = do
  progSectionBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 10 ]
  #append contentBox progSectionBox
  progExpander <- new Gtk.Expander [ #label := tr "Installed Programs", #hexpand := True ]
  #append progSectionBox progExpander
  progBox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical, #spacing := 5, #marginTop := 10 ]
  #setChild progExpander (Just progBox)

  -- Icon widgets of the rows currently displayed. A background extraction
  -- that finishes after a rebuild ("Refresh" clicked meanwhile) updates the
  -- rows that are on screen by looking them up here, instead of writing into
  -- widgets that were removed from the box.
  shownIconsRef <- newIORef [] :: IO (IORef [(FilePath, Gtk.Image)])

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
            programs <- forM lnkFiles $ \path -> do
                let name = T.pack $ takeBaseName path
                let iconPath = applicationIconPath bottle name
                iconExists <- doesFileExist iconPath

                rowBox <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 4 ]

                progBtn <- new Gtk.Button [ #hexpand := True, #halign := Gtk.AlignFill, #tooltipText := T.pack path ]
                progContent <- new Gtk.Box [ #orientation := Gtk.OrientationHorizontal, #spacing := 8 ]
                progIcon <- new Gtk.Image [ #pixelSize := 24, #valign := Gtk.AlignCenter ]
                if iconExists
                  then set progIcon [ #file := T.pack iconPath ]
                  else set progIcon [ #iconName := missingIconName ]
                progLabel <- new Gtk.Label [ #label := name, #hexpand := True, #halign := Gtk.AlignStart ]
                #append progContent progIcon
                #append progContent progLabel
                #setChild progBtn (Just progContent)

                -- With nothing to launch with, the button gets no handler at
                -- all; 'blockIfBlocked' disables it in that case anyway.
                case launchable of
                  Right runnable -> void $ on progBtn #clicked $ runWindowsLnk runnable path
                  Left _         -> pure ()
                #append rowBox progBtn
                blockIfBlocked progBtn

                menuBtn <- buildAppMenuButton bottle launchable name path refreshPrograms
                #append rowBox menuBtn

                #append progBox rowBox
                return (path, iconPath, progIcon, iconExists)
            writeIORef shownIconsRef [ (iconPath, icon) | (_, iconPath, icon, _) <- programs ]
            set progExpander [ #expanded := True ]

            -- One batched invocation for every icon that isn't cached yet
            -- (see 'extractAppIcons'), in its own thread so the list isn't
            -- held up while Wine/Proton starts; the GUI itself is only
            -- touched again through 'GLib.idleAdd'.
            case launchable of
              Left _ -> pure ()
              Right runnable -> do
                let missing = [ (path, iconPath) | (path, iconPath, _, cached) <- programs, not cached ]
                unless (null missing) $ void $ async $ do
                  createdIcons <- extractAppIcons runnable missing
                  unless (null createdIcons) $ void $ GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
                    shownIcons <- readIORef shownIconsRef
                    forM_ createdIcons $ \iconPath ->
                      case lookup iconPath shownIcons of
                        Just icon -> set icon [ #file := T.pack iconPath ]
                        Nothing   -> pure ()
                    return False

  refreshBtn <- new Gtk.Button [ #iconName := "view-refresh-symbolic", #tooltipText := tr "Refresh program list", #valign := Gtk.AlignStart ]
  void $ on refreshBtn #clicked refreshPrograms
  #append progSectionBox refreshBtn
  refreshPrograms
