-- | Domain types and their on-disk JSON representation.
--
module Backup.Types
  ( -- * Applications
    InstanceName
  , isValidInstanceName
  , InstanceConf (..)
  , BackupHost (..)
  , ProxyConf (..)
  , allDays
  , parseDay
    -- * Sitemap
  , SiteMap
  , Resource (..)
  , Etag
  , Source (..)
    -- * Snapshots
  , SnapshotId
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (DayOfWeek (..), LocalTime)

-- | Name of an AtikTeam application. It is used as a directory name and as a
-- URL segment: values coming from outside must pass 'isValidInstanceName'.
type InstanceName = Text

-- | ASCII letters, digits, [.-_], and no leading dot. This is a valid
-- directory name on Linux and Windows, and needs no URL encoding.
isValidInstanceName :: Text -> Bool
isValidInstanceName name =
  not (T.null name)
    && name /= "_"
    && not ("." `T.isPrefixOf` name)
    && T.all validChar name
  where
    validChar c = isAscii c && (isAlphaNum c || c `elem` ("._-" :: String))

-- | A snapshot is identified by the local time at which it was started.
type SnapshotId = LocalTime

data InstanceConf = InstanceConf
  { confHost :: BackupHost
  , confProxy :: Maybe ProxyConf
  , confAuthToken :: Text
  , confBackupDays :: [DayOfWeek]
  , confHistorySize :: Int
  }
  deriving (Eq, Show)

-- | Backup server. All connections use HTTPS.
data BackupHost = BackupHost
  { hostDomain :: Text
  , hostPort :: Int
  , hostPrefix :: Text
  }
  deriving (Eq, Show)

data ProxyConf = ProxyConf
  { proxyHost :: Text
  , proxyPort :: Int
  }
  deriving (Eq, Show)

allDays :: [DayOfWeek]
allDays = [Monday .. Sunday]

-- | Reads a day as written by 'show' (@"Monday"@, ...).
parseDay :: Text -> Maybe DayOfWeek
parseDay name = lookup name [(T.pack (show day), day) | day <- allDays]

-- | A sitemap entry: one version of a file to back up.
data Resource = Resource
  { resEtag :: Etag
  , resSource :: Source
  , resUrl :: Text
  -- ^ path on the backup server ('Internal') or full URL ('External')
  , resPath :: Text
  -- ^ path of the file in the backed-up site
  , resHeaders :: [(Text, Text)]
  -- ^ HTTP headers to send back when browsing the backup
  }
  deriving (Eq, Show)

-- | SHA-256 hash of the content, provided by the server. It is used as the
-- file name in the local database.
type Etag = Text

data Source = Internal | External
  deriving (Eq, Show)

type SiteMap = [Resource]

-- JSON ----------------------------------------------------------------------

instance FromJSON InstanceConf where
  parseJSON = withObject "InstanceConf" $ \o -> do
    days <- o .: "backup-days"
    InstanceConf
      <$> o .: "backup-host"
      <*> o .:? "proxy"
      <*> o .: "auth-token"
      <*> traverse dayFromText days
      <*> o .: "history-size"
    where
      dayFromText :: Text -> Parser DayOfWeek
      dayFromText t = maybe (fail ("unknown day: " <> T.unpack t)) pure (parseDay t)

instance ToJSON InstanceConf where
  toJSON c =
    object
      [ "backup-host" .= confHost c
      , "proxy" .= confProxy c
      , "auth-token" .= confAuthToken c
      , "backup-days" .= map (T.pack . show) (confBackupDays c)
      , "history-size" .= confHistorySize c
      ]

instance FromJSON BackupHost where
  parseJSON = withObject "BackupHost" $ \o ->
    BackupHost <$> o .: "domain" <*> o .: "port" <*> o .: "prefix"

instance ToJSON BackupHost where
  toJSON h =
    object
      [ "domain" .= hostDomain h
      , "port" .= hostPort h
      , "prefix" .= hostPrefix h
      ]

-- | Stored as @[host, port]@.
instance FromJSON ProxyConf where
  parseJSON v = uncurry ProxyConf <$> parseJSON v

instance ToJSON ProxyConf where
  toJSON (ProxyConf host port) = toJSON (host, port)

instance FromJSON Source where
  parseJSON = withText "Source" $ \case
    "internal" -> pure Internal
    "external" -> pure External
    other -> fail ("unknown source type: " <> T.unpack other)

instance ToJSON Source where
  toJSON = \case
    Internal -> "internal"
    External -> "external"

-- | Stored as a positional array
-- @[etag, source, url, path, [[header, value], ...]]@.
instance FromJSON Resource where
  parseJSON v = do
    (etag, source, url, path, headers) <- parseJSON v
    pure (Resource etag source url path headers)

instance ToJSON Resource where
  toJSON (Resource etag source url path headers) =
    toJSON (etag, source, url, path, headers)
