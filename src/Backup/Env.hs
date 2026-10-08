-- | Environment shared by all components: logger, database directory and HTTP
-- connection manager.
module Backup.Env
  ( LogLevel (..)
  , Logger
  , Env (..)
  , stderrLogger
  , logInstance
  , allEnum
  ) where

import Backup.Types (InstanceName)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import Network.HTTP.Client (Manager)
import System.IO (stderr)

data LogLevel = LogInfo | LogNotice | LogError
  deriving (Eq, Ord, Show, Enum, Bounded)

type Logger = LogLevel -> Text -> IO ()

data Env = Env
  { envLog :: Logger
  , envRoot :: FilePath
  , envManager :: Manager
  }

-- | Logs to standard error with a local timestamp, ignoring messages below the
-- given level.
stderrLogger :: LogLevel -> Logger
stderrLogger minLevel level message = when (level >= minLevel) $ do
  now <- getZonedTime
  let stamp = T.pack (formatTime defaultTimeLocale "%F %T" now)
  TIO.hPutStrLn stderr (stamp <> " [" <> tag level <> "] " <> message)
  where
    tag = \case
      LogInfo -> "INFO"
      LogNotice -> "NOTICE"
      LogError -> "ERROR"

-- | Logs a message prefixed with the name of the application concerned.
logInstance :: Env -> InstanceName -> LogLevel -> Text -> IO ()
logInstance env name level message = envLog env level (name <> ": " <> message)

-- | Utility, list all elements from enumerable x bounded
allEnum :: (Bounded a, Enum a) => [a]
allEnum = [minBound .. maxBound]
