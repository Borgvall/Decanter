{-# LANGUAGE RecordWildCards #-}

module Bottle.Logic.Direct3dWrappers
  ( Direct3DWrapperState(..)
  , getDirect3DWrapperState
  , setDirect3DWrapperState
  , direct3DWrapperOverrideDllNames
  , WrapperHealth(..)
  , getDirect3DWrapperHealth
  , repairDirect3DWrapperState
  ) where

import Bottle.Types
import System.Directory
    ( pathIsSymbolicLink
    , doesFileExist
    , removeFile
    , renameFile
    , createFileLink
    , getSymbolicLinkTarget
    )
import System.Environment (lookupEnv)
import System.FilePath ((</>), dropExtension)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch)
import Control.Monad (forM_, when, unless)
import Data.List (isPrefixOf)

-- | The supported states of a wine prefix's Direct3D-to-Vulkan translation
-- setup. "vkd3d-proton only" (without DXVK) is deliberately not modeled:
-- vkd3d-proton's own dxgi.dll dependency comes from DXVK, so it never works
-- without DXVK also being installed.
data Direct3DWrapperState
  = WineD3D -- ^ Wine's own built-in Direct3D implementation (the default).
  | Dxvk    -- ^ DXVK's Direct3D-to-Vulkan translation layer.
  | DxvkAndVkd3dProton -- ^ DXVK and vkd3d-proton, the only combination that works.
  deriving (Show, Eq, Enum, Bounded, Read)

-- | Describes one of the Nix-packaged DLL sets (DXVK, vkd3d-proton) that
-- can be symlinked into a wine prefix.
data WrapperPackage = WrapperPackage
  { wrapperMarkerDll :: String   -- ^ one of 'wrapperDllNames', used to check install status.
  , wrapperDllNames  :: [String]
  , wrapperDir64     :: String   -- ^ subdirectory holding the 64-bit DLLs.
  , wrapperDir32     :: String   -- ^ subdirectory holding the 32-bit DLLs.
  , wrapperEnvVar    :: String   -- ^ env var pointing at the package's Nix store path.
  }

-- | DXVK, via nixpkgs' own "dxvk" package (see flake.nix).
dxvkPackage :: WrapperPackage
dxvkPackage = WrapperPackage
  { wrapperMarkerDll = "dxgi.dll"
  , wrapperDllNames  = ["d3d8.dll", "d3d9.dll", "d3d10core.dll", "d3d11.dll", "dxgi.dll"]
  , wrapperDir64     = "x64"
  , wrapperDir32     = "x32"
  , wrapperEnvVar    = "DECANTER_DXVK_PATH"
  }

-- | vkd3d-proton, via the custom "vkd3dproton-decanter.nix" derivation (see
-- flake.nix) -- nixpkgs' own "vkd3d-proton" package has no usable DLLs.
vkd3dProtonPackage :: WrapperPackage
vkd3dProtonPackage = WrapperPackage
  { wrapperMarkerDll = "d3d12.dll"
  , wrapperDllNames  = ["d3d12.dll", "d3d12core.dll"]
  , wrapperDir64     = "x64"
  , wrapperDir32     = "x86"
  , wrapperEnvVar    = "DECANTER_VKD3D_PROTON_PATH"
  }

-- | Suffix used to back up a wine prefix's original DLL before replacing it
-- with a symlink, so it can be restored when uninstalling again.
backupSuffix :: String
backupSuffix = ".orig-wine"

system32Dir :: Bottle -> FilePath
system32Dir Bottle{..} = bottlePath </> "drive_c" </> "windows" </> "system32"

syswow64Dir :: Bottle -> FilePath
syswow64Dir Bottle{..} = bottlePath </> "drive_c" </> "windows" </> "syswow64"

-- | Wine-prefix directories to manage, paired with whether they hold
-- 64-bit DLLs. A Win64 prefix has both a 64-bit system32 and a 32-bit
-- syswow64; a Win32 prefix only has a (32-bit) system32.
targetDirs :: Bottle -> [(FilePath, Bool)]
targetDirs bottle = case arch bottle of
  Win64 -> [ (system32Dir bottle, True), (syswow64Dir bottle, False) ]
  Win32 -> [ (system32Dir bottle, False) ]

-- | The Nix store path of a wrapper package, exposed via its
-- 'wrapperEnvVar' (see flake.nix: set as a derivation attribute for the
-- test suite's checkPhase, and injected via gappsWrapperArgs for the
-- installed binary at runtime).
getPackageStorePath :: WrapperPackage -> IO FilePath
getPackageStorePath pkg = do
  maybePath <- lookupEnv (wrapperEnvVar pkg)
  case maybePath of
    Just path -> pure path
    Nothing   -> error (wrapperEnvVar pkg ++ " is not set; cannot locate its Nix package.")

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

-- | Whether "pkg"'s DLLs are currently symlinked into "dir" -- checking
-- just one representative DLL is enough, since 'setPackageInstalled'
-- always installs/removes all of a package's DLLs together.
isPackageInstalledIn :: WrapperPackage -> FilePath -> IO Bool
isPackageInstalledIn pkg dir = safePathIsSymbolicLink (dir </> wrapperMarkerDll pkg)

-- | Symlinks a single DLL into "targetDir" (pointing into "sourceDir"),
-- backing up Wine's original file first, unless that already happened
-- during a previous install.
installDll :: FilePath -> FilePath -> String -> IO ()
installDll targetDir sourceDir name = do
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

-- | Removes a DLL symlink from "targetDir" and restores Wine's original
-- file from its backup, if one was made.
uninstallDll :: FilePath -> String -> IO ()
uninstallDll targetDir name = do
  let target = targetDir </> name
      backup = target ++ backupSuffix
  isSymlink <- safePathIsSymbolicLink target
  when isSymlink $ removeFile target
  backupExists <- entryExists backup
  when backupExists $ renameFile backup target

-- | Symlinks all of "pkg"'s DLLs into every relevant directory of "bottle".
installPackage :: Bottle -> WrapperPackage -> IO ()
installPackage bottle pkg = do
  storePath <- getPackageStorePath pkg
  forM_ (targetDirs bottle) $ \(dir, is64) ->
    let sourceDir = storePath </> (if is64 then wrapperDir64 pkg else wrapperDir32 pkg)
    in forM_ (wrapperDllNames pkg) (installDll dir sourceDir)

-- | Removes all of "pkg"'s DLLs from every relevant directory of "bottle".
uninstallPackage :: Bottle -> WrapperPackage -> IO ()
uninstallPackage bottle pkg =
  forM_ (targetDirs bottle) $ \(dir, _) ->
    forM_ (wrapperDllNames pkg) (uninstallDll dir)

-- | Installs or removes "pkg" so that it ends up (not) installed as
-- requested. Does nothing if it already is in that state.
setPackageInstalled :: Bottle -> WrapperPackage -> Bool -> IO ()
setPackageInstalled bottle pkg wanted = do
  isInstalled <- isPackageInstalledIn pkg (system32Dir bottle)
  unless (isInstalled == wanted) $
    if wanted then installPackage bottle pkg else uninstallPackage bottle pkg

-- | Determines a bottle's current Direct3D wrapper state by checking which
-- of DXVK's/vkd3d-proton's DLLs are symlinked in versus Wine's own files.
getDirect3DWrapperState :: Bottle -> IO Direct3DWrapperState
getDirect3DWrapperState bottle = do
  dxvkInstalled <- isPackageInstalledIn dxvkPackage (system32Dir bottle)
  vkd3dProtonInstalled <- isPackageInstalledIn vkd3dProtonPackage (system32Dir bottle)
  pure $ case (dxvkInstalled, vkd3dProtonInstalled) of
    (False, _)    -> WineD3D
    (True, False) -> Dxvk
    (True, True)  -> DxvkAndVkd3dProton

-- | Installs or removes DXVK's/vkd3d-proton's DLLs in "bottle" so that it
-- ends up in "desired" state. Does nothing for a package that's already
-- (not) installed as "desired" requires.
setDirect3DWrapperState :: Bottle -> Direct3DWrapperState -> IO ()
setDirect3DWrapperState bottle desired = do
  setPackageInstalled bottle dxvkPackage (desired /= WineD3D)
  setPackageInstalled bottle vkd3dProtonPackage (desired == DxvkAndVkd3dProton)

-- | Health of a wrapper package's on-disk symlink relative to the Nix store
-- path Decanter is currently configured to install from (see
-- 'getPackageStorePath'). A symlink's target is a fixed absolute Nix store
-- path baked in at install time -- it never follows a Decanter (and thus
-- DXVK/vkd3d-proton) version upgrade on its own.
data WrapperHealth
  = WrapperValid    -- ^ Points at the currently configured Nix store path.
  | WrapperOutdated -- ^ Points at a different, but still existing, Nix store path.
  | WrapperDangling -- ^ Points at a Nix store path that no longer exists (e.g. garbage-collected).
  deriving (Show, Eq)

-- | Health of a single package's symlink in "dir". Assumes the package is
-- actually supposed to be installed there; combine with
-- 'getDirect3DWrapperState' (as 'getDirect3DWrapperHealth' does) if that
-- isn't already known.
getWrapperPackageHealth :: WrapperPackage -> FilePath -> IO WrapperHealth
getWrapperPackageHealth pkg dir = do
  let markerPath = dir </> wrapperMarkerDll pkg
  -- doesFileExist follows symlinks, so this is False for a dangling one.
  targetExists <- doesFileExist markerPath
  if not targetExists
    then pure WrapperDangling
    else do
      currentStorePath <- getPackageStorePath pkg
      linkTarget <- getSymbolicLinkTarget markerPath
      pure $ if currentStorePath `isPrefixOf` linkTarget then WrapperValid else WrapperOutdated

-- | Worst-of health across whichever packages "bottle"'s current Direct3D
-- wrapper state actually has installed. 'WineD3D' has no symlinks at all and
-- is therefore always 'WrapperValid'.
getDirect3DWrapperHealth :: Bottle -> IO WrapperHealth
getDirect3DWrapperHealth bottle = do
  state <- getDirect3DWrapperState bottle
  healths <- mapM (\pkg -> getWrapperPackageHealth pkg (system32Dir bottle)) (packagesFor state)
  pure (worstHealth healths)
  where
    packagesFor WineD3D            = []
    packagesFor Dxvk               = [dxvkPackage]
    packagesFor DxvkAndVkd3dProton = [dxvkPackage, vkd3dProtonPackage]

    worstHealth healths
      | WrapperDangling `elem` healths = WrapperDangling
      | WrapperOutdated `elem` healths = WrapperOutdated
      | otherwise                     = WrapperValid

-- | Unconditionally re-symlinks whichever packages "bottle"'s current
-- Direct3D wrapper state calls for, pointing them at the currently
-- configured Nix store paths. Unlike 'setDirect3DWrapperState', this skips
-- the "already installed" shortcut in 'setPackageInstalled' -- which is
-- exactly what makes it able to repair an outdated or dangling symlink:
-- 'installDll' always relinks unconditionally once actually invoked.
repairDirect3DWrapperState :: Bottle -> IO ()
repairDirect3DWrapperState bottle = do
  state <- getDirect3DWrapperState bottle
  case state of
    WineD3D            -> pure ()
    Dxvk               -> installPackage bottle dxvkPackage
    DxvkAndVkd3dProton  -> installPackage bottle dxvkPackage >> installPackage bottle vkd3dProtonPackage

-- | Base names (without ".dll") of the DLLs that need a Wine "native" DLL
-- override for a bottle to actually behave as "state" claims. Placing the
-- DLL file alone (as 'setDirect3DWrapperState' does) is not enough: Wine's
-- default load order prefers its own builtin implementation for these
-- specific DLLs regardless of what's on disk in system32/syswow64 -- the
-- same reason winetricks' own "dxvk" verb sets a
-- "HKEY_CURRENT_USER\\Software\\Wine\\DllOverrides" registry entry after
-- copying the DLLs. See "Bottle.Logic.Process.getWineOverrides", which
-- turns this into a WINEDLLOVERRIDES environment variable instead of
-- touching the registry.
direct3DWrapperOverrideDllNames :: Direct3DWrapperState -> [String]
direct3DWrapperOverrideDllNames WineD3D             = []
direct3DWrapperOverrideDllNames Dxvk                = dllBaseNames dxvkPackage
direct3DWrapperOverrideDllNames DxvkAndVkd3dProton  =
  dllBaseNames dxvkPackage ++ dllBaseNames vkd3dProtonPackage

dllBaseNames :: WrapperPackage -> [String]
dllBaseNames pkg = map dropExtension (wrapperDllNames pkg)
