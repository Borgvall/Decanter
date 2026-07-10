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

spec :: Spec
spec = describe "Bottle.Logic.Direct3dWrappers" $
  describe "getDirect3DWrapperState / setDirect3DWrapperState" $
    it "walks a System Wine bottle through every state change and no-op" $ withTestEnvironment $ do
      runners <- getAvailableRunners
      maybeDxvkPath <- lookupEnv "DECANTER_DXVK_PATH"
      case (SystemWine `elem` runners, maybeDxvkPath) of
        (False, _) -> pendingWith "No system Wine installation found in this environment; not testable here."
        (_, Nothing) -> pendingWith "DECANTER_DXVK_PATH is not set; enter the Nix dev shell to run this test."
        (True, Just _) -> do
          bottle <- createBottleObject "Direct3dWrapperTestBottle" Win64 SystemWine
          createBottleLogic bottle

          (do
            originalDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")

            -- Fresh prefix: Wine's own Direct3D implementation.
            getDirect3DWrapperState bottle `shouldReturn` WineD3D

            -- No-op: already WineD3D.
            setDirect3DWrapperState bottle WineD3D
            getDirect3DWrapperState bottle `shouldReturn` WineD3D
            pathIsSymbolicLink (system32Dll bottle "dxgi.dll") `shouldReturn` False

            -- Transition: WineD3D -> Dxvk.
            setDirect3DWrapperState bottle Dxvk
            getDirect3DWrapperState bottle `shouldReturn` Dxvk
            pathIsSymbolicLink (system32Dll bottle "dxgi.dll") `shouldReturn` True
            pathIsSymbolicLink (syswow64Dll bottle "dxgi.dll") `shouldReturn` True

            -- No-op: already Dxvk.
            setDirect3DWrapperState bottle Dxvk
            getDirect3DWrapperState bottle `shouldReturn` Dxvk
            pathIsSymbolicLink (system32Dll bottle "dxgi.dll") `shouldReturn` True

            -- Transition: Dxvk -> WineD3D. The original DLL must come back
            -- unchanged, proving the backup/restore round-trip works.
            setDirect3DWrapperState bottle WineD3D
            getDirect3DWrapperState bottle `shouldReturn` WineD3D
            pathIsSymbolicLink (system32Dll bottle "dxgi.dll") `shouldReturn` False
            restoredDxgi <- BS.readFile (system32Dll bottle "dxgi.dll")
            restoredDxgi `shouldBe` originalDxgi
            ) `finally` deleteBottleLogic bottle
