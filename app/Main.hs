module Main (main) where

import qualified Backup.CLI as CLI
import qualified Backup.DB as DB
import Backup.Env
import qualified Backup.Maintenance as Maintenance
import qualified Backup.Migrations as Migrations
import qualified Backup.Server as Server
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, tryPutMVar)
import Control.Monad (void)
import qualified Data.Text as T
import Data.Version (showVersion)
import Network.HTTP.Client.TLS (newTlsManager)
import Paths_atikteam_backup_agent (version)
import System.IO (hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  let versionText = "AtikTeam Backup " <> showVersion version
  opts <- CLI.parseOptions versionText

  root <- maybe DB.getSystemDbRootDir pure (CLI.optDataDir opts)
  -- the root certificates of the operating system are used
  manager <- newTlsManager
  let env =
        Env
          { envLog = stderrLogger (CLI.optLogLevel opts)
          , envRoot = root
          , envManager = manager
          }
  envLog env LogInfo (T.pack versionText)
  envLog env LogInfo ("Database directory: " <> T.pack root)
  Migrations.migrate env

  trigger <- newEmptyMVar
  exitSignal <- newEmptyMVar
  let requestBackup = void (tryPutMVar trigger ())
      -- the delay lets the "shutting down" page be sent first
      requestExit = void (forkIO (threadDelay 300000 >> void (tryPutMVar exitSignal ())))
      serverConfig =
        Server.Config
          { Server.cfgHost = CLI.optHost opts
          , Server.cfgPort = CLI.optPort opts
          , Server.cfgStaticDir = CLI.optStaticDir opts
          }

  putStrLn ("Listening on http://" <> CLI.optHost opts <> ":" <> show (CLI.optPort opts))
  putStrLn "Browse your backups by opening this address in your web browser."

  -- -- If one of the components stops (including on error), the others are
  -- -- stopped too and the error is propagated.
  foldr1 race_ [ takeMVar exitSignal
               , Maintenance.backupLoop env (CLI.optInterval opts) trigger
               , Server.run env serverConfig requestBackup requestExit ]

