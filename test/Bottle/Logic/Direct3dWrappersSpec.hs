{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.Direct3dWrappersSpec (spec) where

import Test.Hspec
import Bottle.Logic.Direct3dWrappers
import Bottle.Logic (getAvailableRunners, createBottleObject, createBottleLogic, deleteBottleLogic)
import Bottle.Types
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, pathIsSymbolicLink)
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
    Both -> do
      isSymlinkedInBothDirs bottle "dxgi.dll" `shouldReturn` True
      isSymlinkedInBothDirs bottle "d3d12.dll" `shouldReturn` True

spec :: Spec
spec = describe "Bottle.Logic.Direct3dWrappers" $
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
          bottle <- createBottleObject "Direct3dWrapperTestBottle" Win64 SystemWine
          createBottleLogic bottle

          (do
            originalDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")
            originalD3d12 <- BS.readFile (system32Dll bottle "d3d12.dll")

            -- Fresh prefix: Wine's own Direct3D implementation.
            assertDirect3DWrapperState bottle WineD3D

            -- Walks an Eulerian circuit of the 3-state transition graph, so
            -- every one of the 3 states' "already there" no-op and every
            -- one of the 6 possible transitions between distinct states is
            -- hit exactly once: no-op WineD3D, WineD3D->Dxvk, no-op Dxvk,
            -- Dxvk->Both, no-op Both, Both->WineD3D, WineD3D->Both,
            -- Both->Dxvk, Dxvk->WineD3D.
            forM_ [WineD3D, Dxvk, Dxvk, Both, Both, WineD3D, Both, Dxvk, WineD3D] $ \state -> do
              setDirect3DWrapperState bottle state
              assertDirect3DWrapperState bottle state

            -- Both DLLs must have made it back to Wine's originals,
            -- byte-for-byte, proving the backup/restore round-trip works.
            restoredDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")
            restoredD3d12 <- BS.readFile (system32Dll bottle "d3d12.dll")
            restoredDxgi `shouldBe` originalDxgi
            restoredD3d12 `shouldBe` originalD3d12
            ) `finally` deleteBottleLogic bottle
