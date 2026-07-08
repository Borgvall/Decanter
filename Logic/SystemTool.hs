module Logic.SystemTool (runSystemTool) where

import System.Process.Typed
import Control.Monad (void)
import System.Environment (getEnvironment)

-- | Führt xdg-open aus, aber bereinigt vorher das Environment von Nix-spezifischen
-- Variablen wie GI_TYPELIB_PATH. Dies verhindert, dass System-Anwendungen (wie Nautilus)
-- abstürzen, weil sie versuchen, inkompatible Bibliotheken aus dem Nix Store zu laden.
runSystemTool :: String -> [String] -> IO ()
runSystemTool tool args = do
  currentEnv <- getEnvironment
  -- Wir filtern GI_TYPELIB_PATH heraus. Dies ist der Hauptverursacher für
  -- "Namespace ... not available" Fehler in Python/GObject-Apps (Nautilus).
  let cleanEnv = filter (\(k, _) -> k /= "GI_TYPELIB_PATH") currentEnv
  void $ startProcess $ setEnv cleanEnv $ proc tool args
