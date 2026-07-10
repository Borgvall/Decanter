{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Direct3dWrappers
  ( Direct3DWrapperState(..)
  , getDirect3DWrapperState
  , setDirect3DWrapperState
  ) where

import Bottle.Types
import System.Directory
    ( pathIsSymbolicLink
    , doesFileExist
    , removeFile
    , renameFile
    , createFileLink
    )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)
import Control.Monad (forM_, when, unless)

-- | The supported states of a wine prefix's Direct3D-to-Vulkan translation
-- setup.
--
-- A third state ("both DXVK and vkd3d-proton installed") is deliberately
-- not modeled yet: nixpkgs' "vkd3d-proton" package only ships native Unix
-- libraries meant for building Wine itself with vkd3d support baked in, not
-- the Windows PE DLLs (d3d12.dll/d3d12core.dll) a wine prefix needs to have
-- vkd3d-proton symlinked in the same way as DXVK below. Sourcing those
-- requires a separate Nix derivation and is tracked as follow-up work.
data Direct3DWrapperState
  = WineD3D -- ^ Wine's own built-in Direct3D implementation (the default).
  | Dxvk    -- ^ DXVK's Direct3D-to-Vulkan translation layer.
  deriving (Show, Eq, Enum, Bounded, Read)

-- | The Direct3D DLLs that DXVK replaces.
dxvkDllNames :: [String]
dxvkDllNames = ["d3d8.dll", "d3d9.dll", "d3d10core.dll", "d3d11.dll", "dxgi.dll"]

-- | Suffix used to back up a wine prefix's original DLL before replacing it
-- with a DXVK symlink, so it can be restored when switching back to WineD3D.
backupSuffix :: String
backupSuffix = ".orig-wine"

system32Dir :: Bottle -> FilePath
system32Dir Bottle{..} = bottlePath </> "drive_c" </> "windows" </> "system32"

syswow64Dir :: Bottle -> FilePath
syswow64Dir Bottle{..} = bottlePath </> "drive_c" </> "windows" </> "syswow64"

-- | Wine-prefix directories to manage, paired with the matching DXVK
-- bitness subdirectory ("x64" for 64-bit DLLs, "x32" for 32-bit ones). A
-- Win64 prefix has both a 64-bit system32 and a 32-bit syswow64; a Win32
-- prefix only has a (32-bit) system32.
targetDirsWithBitness :: Bottle -> [(FilePath, FilePath)]
targetDirsWithBitness bottle = case arch bottle of
  Win64 -> [ (system32Dir bottle, "x64"), (syswow64Dir bottle, "x32") ]
  Win32 -> [ (system32Dir bottle, "x32") ]

-- | The Nix store path of the "dxvk" package, exposed via the
-- DECANTER_DXVK_PATH environment variable (see flake.nix: set as a
-- derivation attribute for the test suite's checkPhase, and injected via
-- gappsWrapperArgs for the installed binary at runtime).
getDxvkStorePath :: IO FilePath
getDxvkStorePath = do
  maybePath <- lookupEnv "DECANTER_DXVK_PATH"
  case maybePath of
    Just path -> pure path
    Nothing   -> error "DECANTER_DXVK_PATH is not set; cannot locate the DXVK Nix package."

-- | Like 'pathIsSymbolicLink', but returns False instead of throwing when
-- "path" does not exist at all.
safePathIsSymbolicLink :: FilePath -> IO Bool
safePathIsSymbolicLink path =
  pathIsSymbolicLink path `catch` \e ->
    if isDoesNotExistError e then pure False else ioError e

-- | Whether a filesystem entry (regular file or symlink, even a dangling
-- one) exists at "path".
entryExists :: FilePath -> IO Bool
entryExists path = do
  isLink <- safePathIsSymbolicLink path
  if isLink then pure True else doesFileExist path

-- | Whether DXVK's DLLs are currently symlinked into "dir" -- used as a
-- representative marker for the whole prefix, since
-- 'setDirect3DWrapperState' always installs/removes all DLLs in all
-- relevant directories together.
isDxvkInstalledIn :: FilePath -> IO Bool
isDxvkInstalledIn dir = safePathIsSymbolicLink (dir </> "dxgi.dll")

-- | Symlinks a single DXVK DLL into "targetDir" (pointing into "sourceDir",
-- i.e. one of DXVK's own "x64"/"x32" directories), backing up Wine's
-- original file first, unless that already happened during a previous
-- install.
installDxvkDll :: FilePath -> FilePath -> String -> IO ()
installDxvkDll targetDir sourceDir name = do
  let target = targetDir </> name
      backup = target ++ backupSuffix
  isSymlink <- safePathIsSymbolicLink target
  if isSymlink
    then removeFile target
    else do
      targetExists <- doesFileExist target
      backupExists <- entryExists backup
      case (targetExists, backupExists) of
        (True, False) -> renameFile target backup
        (True, True)  -> removeFile target
        (False, _)    -> pure ()
  createFileLink (sourceDir </> name) target

-- | Removes a DXVK DLL symlink from "targetDir" and restores Wine's
-- original file from its backup, if one was made.
uninstallDxvkDll :: FilePath -> String -> IO ()
uninstallDxvkDll targetDir name = do
  let target = targetDir </> name
      backup = target ++ backupSuffix
  isSymlink <- safePathIsSymbolicLink target
  when isSymlink $ removeFile target
  backupExists <- entryExists backup
  when backupExists $ renameFile backup target

-- | Determines a bottle's current Direct3D wrapper state by checking
-- whether its Direct3D DLLs are DXVK symlinks or Wine's own files.
getDirect3DWrapperState :: Bottle -> IO Direct3DWrapperState
getDirect3DWrapperState bottle = do
  dxvkInstalled <- isDxvkInstalledIn (system32Dir bottle)
  pure $ if dxvkInstalled then Dxvk else WineD3D

-- | Installs or removes DXVK's DLLs in "bottle" so that it ends up in
-- "desired" state. Does nothing if "bottle" is already in that state.
setDirect3DWrapperState :: Bottle -> Direct3DWrapperState -> IO ()
setDirect3DWrapperState bottle desired = do
  current <- getDirect3DWrapperState bottle
  unless (current == desired) $ case desired of
    WineD3D ->
      forM_ (targetDirsWithBitness bottle) $ \(dir, _) ->
        forM_ dxvkDllNames (uninstallDxvkDll dir)
    Dxvk -> do
      dxvkPath <- getDxvkStorePath
      forM_ (targetDirsWithBitness bottle) $ \(dir, bitness) ->
        forM_ dxvkDllNames (installDxvkDll dir (dxvkPath </> bitness))
