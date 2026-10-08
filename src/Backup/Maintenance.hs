-- | Backup cycle: finds the applications to back up, takes their snapshots,
-- deletes the oldest ones, then removes the files that became useless.
module Backup.Maintenance
  ( backupLoop
  , needsBackup
  , expiredSnapshots
  ) where

import qualified Backup.DB as DB
import Backup.Env
import Backup.Retrieve (snapshotInstance)
import Backup.Time (getLocalTime)
import Backup.Types
import Control.Concurrent.MVar (MVar, takeMVar)
import Control.Exception (displayException)
import Control.Exception.Safe (catchAny, handleAny)
import Control.Monad (forever, void, when)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time (LocalTime, dayOfWeek, localDay)
import System.Timeout (timeout)

-- | Runs a cycle right away, then a new one on every manual trigger (the
-- @MVar@ being filled) or, failing that, after @interval@ seconds.
backupLoop :: Env -> Int -> MVar () -> IO ()
backupLoop env interval trigger = forever $ do
  runCycle env `catchAny` \err ->
    envLog env LogError ("Backup cycle interrupted: " <> T.pack (displayException err))
  void (timeout (interval * 1000000) (takeMVar trigger))

runCycle :: Env -> IO ()
runCycle env = do
  envLog env LogInfo "Checking which applications need a backup"
  now <- getLocalTime
  names <- DB.listInstances (envRoot env)
  -- a failure on one application must not prevent backing up the others
  for_ names $ \name ->
    handleAny (reportFailure name) (refreshIfDue env now name)
  where
    reportFailure name err =
      logInstance env name LogError ("backup failed: " <> T.pack (displayException err))

refreshIfDue :: Env -> LocalTime -> InstanceName -> IO ()
refreshIfDue env now name = do
  let root = envRoot env
  conf <- DB.loadInstance root name
  lastBackup <- listToMaybe <$> DB.listSnapshots root name
  when (needsBackup now conf lastBackup) $ refreshInstance env name conf
  

-- | A backup is due if it is scheduled today and none has been made yet today.
needsBackup :: LocalTime -> InstanceConf -> Maybe SnapshotId -> Bool
needsBackup now conf lastBackup =
  dayOfWeek today `elem` confBackupDays conf
  && case lastBackup of
       Nothing -> True
       Just d -> localDay d < today
  where
    today = localDay now

-- | Snapshots to delete so that only the @size@ most recent ones are kept (at
-- least one).
expiredSnapshots :: Int -> [SnapshotId] -> [SnapshotId]
expiredSnapshots size = drop (max 1 size) . sortOn Down

-- | Updates the backup of an application.
refreshInstance :: Env -> InstanceName -> InstanceConf -> IO ()
refreshInstance env name conf = do
  let root = envRoot env
      say = logInstance env name
  say LogNotice "starting backup update ..."
  snapshotInstance env name conf
  snapshots <- DB.listSnapshots root name
  for_ (expiredSnapshots (confHistorySize conf) snapshots) (DB.deleteSnapshot root name)
  removed <- DB.garbageCollect root name
  say LogInfo (T.pack (show (length removed)) <> " obsolete file(s) removed")
  say LogNotice "... backup update finished"
