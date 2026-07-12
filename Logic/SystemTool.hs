module Logic.SystemTool (runSystemTool) where

import System.Process.Typed
import Control.Monad (void)
import System.Environment (getEnvironment)

-- | Runs xdg-open, but first cleans the environment of Nix-specific
-- variables like GI_TYPELIB_PATH. This prevents system applications (like
-- Nautilus) from crashing because they try to load incompatible libraries
-- from the Nix store.
runSystemTool :: String -> [String] -> IO ()
runSystemTool tool args = do
  currentEnv <- getEnvironment
  -- Filter out GI_TYPELIB_PATH, the main cause of "Namespace ... not
  -- available" errors in Python/GObject apps (Nautilus).
  let cleanEnv = filter (\(k, _) -> k /= "GI_TYPELIB_PATH") currentEnv
  void $ startProcess $ setEnv cleanEnv $ proc tool args
