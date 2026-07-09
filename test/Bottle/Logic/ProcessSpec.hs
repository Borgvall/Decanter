{-# LANGUAGE OverloadedStrings #-}

module Bottle.Logic.ProcessSpec (spec) where

import Test.Hspec
import Bottle.Logic.Process
import Bottle.Logic (getAvailableRunners, createBottleObject, createBottleLogic, deleteBottleLogic, runCmd)
import Bottle.Types
import System.Directory (createDirectoryIfMissing, findExecutable, getCurrentDirectory)
import System.FilePath ((</>))
import System.Environment (setEnv, unsetEnv)
import qualified System.Process.Typed as PT
import System.Exit (ExitCode(..))
import Control.Exception (finally, try, SomeException)
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf, partition)

-- | Whether any host process' command line contains "needle" (via pgrep -f).
-- Used to check for a running/stopped Windows process started via 'runCmd':
-- unlike Proton (containerized via pressure-vessel, see 'findBottleScopes'),
-- System Wine processes are directly visible on the host process tree.
isProcessRunning :: String -> IO Bool
isProcessRunning needle = do
  (exitCode, _) <- PT.readProcessStdout $ PT.proc "pgrep" ["-f", needle]
  pure (exitCode == ExitSuccess)

-- | Test helper: sets up an isolated test environment (a dedicated
-- XDG_DATA_HOME under the build directory, separate from the user's real
-- bottles). Also disables umu-run's runtime update check
-- (UMU_RUNTIME_UPDATE=0), which otherwise makes an HTTP request on every
-- single invocation and would make Proton-based tests slow and
-- network-dependent.
--
-- Unlike the bottle data itself, this directory is deliberately NOT wiped
-- after the test: umu-run downloads several hundred MB of runtime into it
-- on first use, and re-downloading that on every test run would make the
-- suite unusably slow. Only this test's own bottle (already removed via
-- 'deleteBottleLogic') needs to disappear between runs.
withTestEnvironment :: IO () -> IO ()
withTestEnvironment action = do
    cwd <- getCurrentDirectory
    let xdgDataHome = cwd </> "dist-newstyle" </> "decanter-test-xdg-data-home"

    createDirectoryIfMissing True xdgDataHome
    setEnv "XDG_DATA_HOME" xdgDataHome
    setEnv "UMU_RUNTIME_UPDATE" "0"

    action `finally` do
        unsetEnv "XDG_DATA_HOME"
        unsetEnv "UMU_RUNTIME_UPDATE"

-- | Polls "check" once a second, up to "attempts" times, until it returns True.
waitUntil :: Int -> IO Bool -> IO Bool
waitUntil attempts check
  | attempts <= 0 = check
  | otherwise = do
      ok <- check
      if ok
        then pure True
        else threadDelay 1000000 >> waitUntil (attempts - 1) check

-- | Command line of the long-running Windows "ping" started by
-- 'startLongRunningPing' below; also used to recognize it in process
-- listings (e.g. via 'isProcessRunning').
pingMarker :: String
pingMarker = "ping -n 240 127.0.0.1"

-- | Starts a Windows "ping" inside the bottle that runs long enough (240
-- iterations, one per second) for the kill tests below to reliably catch it
-- still running before they try to kill it.
startLongRunningPing :: Bottle -> IO ()
startLongRunningPing bottle = runCmd bottle "wine" ["cmd.exe", "/c", pingMarker]

-- | Runs "assertions", then always kills any leftover processes and deletes
-- "bottle" -- even if an assertion above failed -- so a failing test doesn't
-- leave a Windows process running in the background for the rest of the
-- suite, or a stale bottle blocking the next run.
withBottleCleanup :: Bottle -> IO () -> IO ()
withBottleCleanup bottle assertions =
  assertions `finally` do
    _ <- try (killBottleProcesses bottle) :: IO (Either SomeException ())
    deleteBottleLogic bottle

spec :: Spec
spec = describe "Bottle.Logic.Process" $ do

  describe "getMergedWineEnv" $ do
    it "sets WINEPREFIX and WINEARCH according to the bottle" $ do
      let bottle = Bottle "Test" "/tmp/decanter-test-prefix" SystemWine Win64
      env <- getMergedWineEnv bottle
      lookup "WINEPREFIX" env `shouldBe` Just "/tmp/decanter-test-prefix"
      lookup "WINEARCH" env `shouldBe` Just "win64"
      lookup "PROTONPATH" env `shouldBe` Nothing

    it "sets PROTONPATH when using a Proton runner" $ do
      let bottle = Bottle "Test" "/tmp/decanter-test-prefix" (Proton "/opt/GE-Proton") Win64
      env <- getMergedWineEnv bottle
      lookup "PROTONPATH" env `shouldBe` Just "/opt/GE-Proton"

    it "sets PRESSURE_VESSEL_SYSTEMD_SCOPE for a Proton runner, but not for System Wine" $ do
      let protonBottle = Bottle "Test" "/tmp/decanter-test-prefix" (Proton "/opt/GE-Proton") Win64
      protonEnv <- getMergedWineEnv protonBottle
      lookup "PRESSURE_VESSEL_SYSTEMD_SCOPE" protonEnv `shouldBe` Just "1"

      let systemWineBottle = protonBottle { runner = SystemWine }
      systemWineEnv <- getMergedWineEnv systemWineBottle
      lookup "PRESSURE_VESSEL_SYSTEMD_SCOPE" systemWineEnv `shouldBe` Nothing

  describe "md5Hex" $ do
    it "matches known MD5 test vectors" $ do
      md5Hex "" `shouldReturn` "d41d8cd98f00b204e9800998ecf8427e"
      md5Hex "abc" `shouldReturn` "900150983cd24fb0d6963f7d28e17f72"

  describe "killBottleProcesses" $ do
    it "kills a long-running Windows process (ping) inside a System Wine bottle" $ withTestEnvironment $ do
      runners <- getAvailableRunners
      case SystemWine `elem` runners of
        False -> pendingWith "No system Wine installation found in this environment; not testable here."
        True -> do
          let bottle = Bottle "SystemWineKillTestBottle" "/tmp/decanter-test-systemwine-kill-prefix" SystemWine Win64
          createBottleLogic bottle
          startLongRunningPing bottle

          withBottleCleanup bottle $ do
            started <- waitUntil 30 (isProcessRunning pingMarker)
            started `shouldBe` True

            killBottleProcesses bottle

            stopped <- waitUntil 10 (not <$> isProcessRunning pingMarker)
            stopped `shouldBe` True

    it "kills a long-running Windows process (ping) inside a Proton bottle" $ withTestEnvironment $ do
      runners <- getAvailableRunners
      maybeUmuRun <- findExecutable "umu-run"
      -- Prefer a "dwproton" install if one is available: it's a much
      -- lighter compatibility tool than a full GE-Proton build, so the
      -- test's container boot is noticeably faster.
      let (dwprotonPaths, otherProtonPaths) = partition ("dwproton" `isInfixOf`) [ p | Proton p <- runners ]
      case (maybeUmuRun, dwprotonPaths ++ otherProtonPaths) of
        (Just _, protonPath : _) -> do
          bottle <- createBottleObject "ProtonKillTestBottle" Win64 (Proton protonPath)
          createBottleLogic bottle
          startLongRunningPing bottle

          withBottleCleanup bottle $ do
            -- A fresh pressure-vessel container takes a while to boot,
            -- so poll generously before giving up.
            started <- waitUntil 120 (not . null <$> findBottleScopes bottle)
            started `shouldBe` True

            killBottleProcesses bottle

            stopped <- waitUntil 30 (null <$> findBottleScopes bottle)
            stopped `shouldBe` True
        _ -> pendingWith "No Proton installation and/or umu-run executable found in this environment; not testable here."
