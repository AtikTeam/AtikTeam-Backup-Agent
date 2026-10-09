-- | Snapshot naming.
--
-- A snapshot is named after the local time at which it was started, in a
-- format that sorts chronologically and contains no character that is
-- forbidden in Windows file names. The format is the one used since the first
-- version, so existing databases need no migration.
-- TODO : merge into an utility module
module Backup.Time
  ( getLocalTime
  , snapshotName
  , parseSnapshotName
  ) where

import Data.Time
  ( LocalTime
  , defaultTimeLocale
  , formatTime
  , getZonedTime
  , parseTimeM
  , zonedTimeToLocalTime
  )

getLocalTime :: IO LocalTime
getLocalTime = zonedTimeToLocalTime <$> getZonedTime

snapshotFormat :: String
snapshotFormat = "%Y-%m-%d---%H-%M-%S"

snapshotName :: LocalTime -> String
snapshotName = formatTime defaultTimeLocale snapshotFormat

parseSnapshotName :: String -> Maybe LocalTime
parseSnapshotName = parseTimeM False defaultTimeLocale snapshotFormat
