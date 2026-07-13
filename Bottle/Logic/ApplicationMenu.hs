{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ApplicationMenu
  ( addToApplicationMenu
  , removeFromApplicationMenu
  , isInApplicationMenu
  , removeApplicationMenuSymlink
  ) where

import Bottle.Types
import Bottle.Logic.Process (extractAppIcon)
import System.Directory
    ( createDirectoryIfMissing
    , getXdgDirectory
    , XdgDirectory(XdgData)
    , doesFileExist
    , doesPathExist
    , removeFile
    , createFileLink
    )
import System.FilePath ((</>))
import Control.Exception (try, IOException)
import Control.Monad (when)
import qualified Data.Text as T

-- Application Menu Integration
--
-- The ".desktop" files are deliberately created *inside* the bottle
-- directory (in "menu/") instead of directly in
-- ~/.local/share/applications: since the bottle directory is its own BTRFS
-- subvolume, they automatically travel along with every snapshot/restore
-- and stay consistent with the actually installed program state. In
-- ~/.local/share/applications there is only a single symlink per bottle,
-- pointing at this "menu" directory (per the Desktop Entry Spec,
-- subdirectories under "applications/" are recursively recognized as
-- desktop file IDs -- Wine's own winemenubuilder does the same).

-- | Directory inside the bottle where the ".desktop" files live.
bottleMenuDir :: Bottle -> FilePath
bottleMenuDir Bottle{..} = bottlePath </> "menu"

-- | Path of an application's ".desktop" file inside the bottle.
desktopFilePath :: Bottle -> T.Text -> FilePath
desktopFilePath bottle appName = bottleMenuDir bottle </> T.unpack appName ++ ".desktop"

-- | Path of an application's (best-effort extracted) icon inside the
-- bottle. Lives in the "menu" directory just like the ".desktop" file
-- itself, so it travels along with snapshots too.
iconFilePath :: Bottle -> T.Text -> FilePath
iconFilePath bottle appName = bottleMenuDir bottle </> "icons" </> T.unpack appName ++ ".png"

-- | Name of the symlink in ~/.local/share/applications for a bottle.
applicationMenuSymlinkName :: Bottle -> String
applicationMenuSymlinkName Bottle{..} = "decanter-" ++ T.unpack bottleName

-- | Escapes an Exec parameter per the Desktop Entry Spec quoting rules
-- (inside double quotes, ", `, $ and \ must be backslash-escaped) and
-- wraps it in double quotes.
quoteExecArg :: T.Text -> T.Text
quoteExecArg arg = "\"" <> T.concatMap escapeChar arg <> "\""
  where
    escapeChar c
      | c `elem` ("\"`$\\" :: String) = T.pack ['\\', c]
      | otherwise = T.singleton c

-- | Ensures ~/.local/share/applications/decanter-<bottle> points at the
-- bottle's "menu" directory. Idempotent.
ensureApplicationMenuSymlink :: Bottle -> IO ()
ensureApplicationMenuSymlink bottle = do
  appsDir <- getXdgDirectory XdgData "applications"
  createDirectoryIfMissing True appsDir
  let linkPath = appsDir </> applicationMenuSymlinkName bottle
  exists <- doesPathExist linkPath
  if exists
    then return ()
    else do
      result <- try (createFileLink (bottleMenuDir bottle) linkPath) :: IO (Either IOException ())
      case result of
        Right () -> return ()
        Left _   -> return () -- race with a concurrent call; the target then already exists

-- | Removes a bottle's application-menu symlink, if present.
removeApplicationMenuSymlink :: Bottle -> IO ()
removeApplicationMenuSymlink bottle = do
  appsDir <- getXdgDirectory XdgData "applications"
  let linkPath = appsDir </> applicationMenuSymlinkName bottle
  exists <- doesPathExist linkPath
  when exists $ removeFile linkPath

-- | Creates an application-menu entry for a start-menu application. The
-- entry calls "decanter start <bottle> <app>", so it runs through
-- Decanter's own execution logic (env merging, Proton routing, Direct3D
-- wrapper) instead of invoking Wine/the application directly.
--
-- The icon is extracted from the ".lnk" file via 'extractAppIcon' (best
-- effort, see there): if that fails, the entry simply gets no "Icon="
-- field, instead of the whole addition failing.
addToApplicationMenu :: Bottle -> T.Text -> FilePath -> T.Text -> IO ()
addToApplicationMenu bottle appName lnkPath category = do
  createDirectoryIfMissing True (bottleMenuDir bottle)
  createDirectoryIfMissing True (bottleMenuDir bottle </> "icons")
  ensureApplicationMenuSymlink bottle

  let iconPath = iconFilePath bottle appName
  iconExtracted <- extractAppIcon bottle lnkPath iconPath
  let iconLine = if iconExtracted then ["Icon=" <> T.pack iconPath] else []

  writeFile (desktopFilePath bottle appName) $ T.unpack $ T.unlines $
    [ "[Desktop Entry]"
    , "Type=Application"
    , "Name=" <> appName
    , "Exec=decanter start " <> quoteExecArg (bottleName bottle) <> " " <> quoteExecArg appName
    , "Categories=" <> category <> ";"
    , "Terminal=false"
    ] ++ iconLine

-- | Removes a previously created application-menu entry again (including
-- its icon, if one was extracted).
removeFromApplicationMenu :: Bottle -> T.Text -> IO ()
removeFromApplicationMenu bottle appName = do
  let path = desktopFilePath bottle appName
  exists <- doesFileExist path
  when exists $ removeFile path

  let iconPath = iconFilePath bottle appName
  iconExists <- doesFileExist iconPath
  when iconExists $ removeFile iconPath

-- | Checks whether an application-menu entry already exists for an application.
isInApplicationMenu :: Bottle -> T.Text -> IO Bool
isInApplicationMenu bottle appName = doesFileExist (desktopFilePath bottle appName)
