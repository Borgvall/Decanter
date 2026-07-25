{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Direct3dWrappersSpec (spec) where

import Test.Hspec
import Bottle.Logic.Direct3dWrappers
import Bottle.Logic (createBottleObject)
import Bottle.Logic.TestSupport (withTestBottle)
import Bottle.Logic.Runner (getAvailableRunners)
import Bottle.Types
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, pathIsSymbolicLink, removeFile, createFileLink)
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))
import Control.Exception (finally)
import Control.Monad (forM_)
import qualified Data.ByteString as BS

-- | Test helper: sets up an isolated XDG_DATA_HOME, separate from the
-- user's real bottles. See Bottle.Logic.ProcessSpec.withTestEnvironment for
-- why this directory is not wiped after the test.
withTestEnvironment :: IO () -> IO ()
withTestEnvironment action = do
    cwd <- getCurrentDirectory
    let xdgDataHome = cwd </> "dist-newstyle" </> "decanter-test-xdg-data-home"

    createDirectoryIfMissing True xdgDataHome
    setEnv "XDG_DATA_HOME" xdgDataHome

    action `finally` unsetEnv "XDG_DATA_HOME"

system32Dll :: Bottle -> String -> FilePath
system32Dll bottle name = bottlePath bottle </> "drive_c" </> "windows" </> "system32" </> name

syswow64Dll :: Bottle -> String -> FilePath
syswow64Dll bottle name = bottlePath bottle </> "drive_c" </> "windows" </> "syswow64" </> name

-- | Whether "name" is symlinked in both system32 and syswow64 of "bottle".
isSymlinkedInBothDirs :: Bottle -> String -> IO Bool
isSymlinkedInBothDirs bottle name = do
  inSystem32 <- pathIsSymbolicLink (system32Dll bottle name)
  inSyswow64 <- pathIsSymbolicLink (syswow64Dll bottle name)
  pure (inSystem32 && inSyswow64)

-- | Whether "name" is still Wine's own file (not a symlink) in both
-- directories of "bottle".
isWineOwnInBothDirs :: Bottle -> String -> IO Bool
isWineOwnInBothDirs bottle name = do
  inSystem32 <- pathIsSymbolicLink (system32Dll bottle name)
  inSyswow64 <- pathIsSymbolicLink (syswow64Dll bottle name)
  pure (not inSystem32 && not inSyswow64)

-- | Asserts "bottle" is really in "state": both the reported state and the
-- on-disk symlink status of a DXVK marker DLL (dxgi.dll) and a
-- vkd3d-proton marker DLL (d3d12.dll) must agree.
assertDirect3DWrapperState :: Bottle -> Direct3DWrapperState -> Expectation
assertDirect3DWrapperState bottle state = do
  getDirect3DWrapperState bottle `shouldReturn` state
  case state of
    WineD3D -> do
      isWineOwnInBothDirs bottle "dxgi.dll" `shouldReturn` True
      isWineOwnInBothDirs bottle "d3d12.dll" `shouldReturn` True
    Dxvk -> do
      isSymlinkedInBothDirs bottle "dxgi.dll" `shouldReturn` True
      isWineOwnInBothDirs bottle "d3d12.dll" `shouldReturn` True
    DxvkAndVkd3dProton -> do
      isSymlinkedInBothDirs bottle "dxgi.dll" `shouldReturn` True
      isSymlinkedInBothDirs bottle "d3d12.dll" `shouldReturn` True

spec :: Spec
spec = describe "Bottle.Logic.Direct3dWrappers" $ do

  describe "direct3DWrapperOverrideDllNames" $ do
    it "lists no DLLs to override for WineD3D" $
      direct3DWrapperOverrideDllNames WineD3D `shouldBe` []

    it "lists DXVK's DLLs (without the .dll extension) for Dxvk" $
      direct3DWrapperOverrideDllNames Dxvk
        `shouldBe` ["d3d8", "d3d9", "d3d10core", "d3d11", "dxgi"]

    it "lists both DXVK's and vkd3d-proton's DLLs for DxvkAndVkd3dProton" $
      direct3DWrapperOverrideDllNames DxvkAndVkd3dProton
        `shouldBe` ["d3d8", "d3d9", "d3d10core", "d3d11", "dxgi", "d3d12", "d3d12core"]

  describe "getDirect3DWrapperState / setDirect3DWrapperState" $
    it "walks a System Wine bottle through all six state changes and three no-ops" $ withTestEnvironment $ do
      runners <- getAvailableRunners
      maybeDxvkPath <- lookupEnv "DECANTER_DXVK_PATH"
      maybeVkd3dProtonPath <- lookupEnv "DECANTER_VKD3D_PROTON_PATH"
      case (SystemWine `elem` runners, maybeDxvkPath, maybeVkd3dProtonPath) of
        (False, _, _) -> pendingWith "No system Wine installation found in this environment; not testable here."
        (_, Nothing, _) -> pendingWith "DECANTER_DXVK_PATH is not set; enter the Nix dev shell to run this test."
        (_, _, Nothing) -> pendingWith "DECANTER_VKD3D_PROTON_PATH is not set; enter the Nix dev shell to run this test."
        (True, Just _, Just _) -> do
          bottle <- createBottleObject "Direct3dWrapperTestBottle" SystemWine
          withTestBottle bottle $ \_ -> do
            originalDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")
            originalD3d12 <- BS.readFile (system32Dll bottle "d3d12.dll")

            -- Fresh prefix: Wine's own Direct3D implementation.
            assertDirect3DWrapperState bottle WineD3D

            -- Walks an Eulerian circuit of the 3-state transition graph, so
            -- every one of the 3 states' "already there" no-op and every
            -- one of the 6 possible transitions between distinct states is
            -- hit exactly once: no-op WineD3D, WineD3D->Dxvk, no-op Dxvk,
            -- Dxvk->DxvkAndVkd3dProton, no-op DxvkAndVkd3dProton,
            -- DxvkAndVkd3dProton->WineD3D, WineD3D->DxvkAndVkd3dProton,
            -- DxvkAndVkd3dProton->Dxvk, Dxvk->WineD3D.
            forM_ [ WineD3D, Dxvk, Dxvk, DxvkAndVkd3dProton, DxvkAndVkd3dProton
                  , WineD3D, DxvkAndVkd3dProton, Dxvk, WineD3D
                  ] $ \state -> do
              setDirect3DWrapperState bottle state
              assertDirect3DWrapperState bottle state

            -- Both DLLs must have made it back to Wine's originals,
            -- byte-for-byte, proving the backup/restore round-trip works.
            restoredDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")
            restoredD3d12 <- BS.readFile (system32Dll bottle "d3d12.dll")
            restoredDxgi `shouldBe` originalDxgi
            restoredD3d12 `shouldBe` originalD3d12

  describe "getDirect3DWrapperHealth / repairDirect3DWrapperState" $
    it "detects an outdated/dangling symlink and repairs it back to valid" $ withTestEnvironment $ do
      runners <- getAvailableRunners
      maybeDxvkPath <- lookupEnv "DECANTER_DXVK_PATH"
      maybeVkd3dProtonPath <- lookupEnv "DECANTER_VKD3D_PROTON_PATH"
      case (SystemWine `elem` runners, maybeDxvkPath, maybeVkd3dProtonPath) of
        (False, _, _) -> pendingWith "No system Wine installation found in this environment; not testable here."
        (_, Nothing, _) -> pendingWith "DECANTER_DXVK_PATH is not set; enter the Nix dev shell to run this test."
        (_, _, Nothing) -> pendingWith "DECANTER_VKD3D_PROTON_PATH is not set; enter the Nix dev shell to run this test."
        (True, Just _, Just _) -> do
          bottle <- createBottleObject "Direct3dWrapperHealthTestBottle" SystemWine
          withTestBottle bottle $ \_ -> do
            setDirect3DWrapperState bottle DxvkAndVkd3dProton
            getDirect3DWrapperHealth bottle `shouldReturn` WrapperValid
            getDirect3DWrapperStatus bottle `shouldReturn` WrapperManaged WrapperValid
            isBottleReadyForWindowsApps bottle `shouldReturn` True

            -- Simulate a symlink left over from an older Decanter/DXVK
            -- version: still resolves to a real file, just not the one the
            -- currently configured DECANTER_DXVK_PATH points at.
            let dxgiPath = system32Dll bottle "dxgi.dll"
                decoyPath = bottlePath bottle </> "decoy-dxgi.dll"
            writeFile decoyPath "decoy"
            removeFile dxgiPath
            createFileLink decoyPath dxgiPath
            getDirect3DWrapperHealth bottle `shouldReturn` WrapperOutdated
            getDirect3DWrapperStatus bottle `shouldReturn` WrapperManaged WrapperOutdated
            isBottleReadyForWindowsApps bottle `shouldReturn` True

            -- Simulate the store path having been garbage-collected since.
            removeFile dxgiPath
            createFileLink (bottlePath bottle </> "does-not-exist.dll") dxgiPath
            getDirect3DWrapperHealth bottle `shouldReturn` WrapperDangling
            getDirect3DWrapperStatus bottle `shouldReturn` WrapperManaged WrapperDangling
            isBottleReadyForWindowsApps bottle `shouldReturn` False

            -- Repairing must restore a healthy symlink without changing the
            -- nominal state (still DxvkAndVkd3dProton).
            repairDirect3DWrapperState bottle
            getDirect3DWrapperHealth bottle `shouldReturn` WrapperValid
            isBottleReadyForWindowsApps bottle `shouldReturn` True
            assertDirect3DWrapperState bottle DxvkAndVkd3dProton

  describe "getDirect3DWrapperStatus / isBottleReadyForWindowsApps" $
    it "reports a Proton bottle as unmanaged and always ready" $ withTestEnvironment $ do
      -- Proton brings its own DXVK/vkd3d-proton, so this must hold without
      -- requiring an actual Proton install -- createBottleObject only
      -- builds the Bottle record and doesn't create the prefix itself, but
      -- still needs a writable XDG_DATA_HOME (see 'withTestEnvironment')
      -- for its own bookkeeping directory.
      bottle <- createBottleObject "Direct3dWrapperStatusProtonTestBottle" (Proton "/Test/Path")
      getDirect3DWrapperStatus bottle `shouldReturn` WrapperNotManaged
      isBottleReadyForWindowsApps bottle `shouldReturn` True
