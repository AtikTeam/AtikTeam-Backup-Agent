-- | Migrations of the database format.

module Backup.Migrations (migrate) where

import qualified Backup.DB as DB
import Backup.Env
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

latestVersion :: Int
latestVersion = 1

-- | Brings the database up to date, one step at a time. The version file is
-- updated after each step.
migrate :: Env -> IO ()
migrate env = do
  let root = envRoot env
  current <- readVersion root
  case compare current latestVersion of
    GT -> fail "this version of the program cannot read the database (format too recent)"
    EQ -> envLog env LogInfo "Database format is up to date"
    LT -> for_ [current + 1 .. latestVersion] $ \step -> do
      envLog env LogInfo ("Migrating database to format " <> T.pack (show step))
      migrateToStep root step
      TIO.writeFile (versionFile root) (T.pack (show step))

migrateToStep :: FilePath -> Int -> IO ()
migrateToStep root = \case
  1 -> createDirectoryIfMissing True (DB.instancesDir root)
  step -> fail ("unknown database migration: " <> show step)

-- | Version of the format; 0 for a database that does not exist yet.
readVersion :: FilePath -> IO Int
readVersion root = do
  exists <- doesFileExist (versionFile root)
  if not exists
    then pure 0
    else do
      content <- TIO.readFile (versionFile root)
      maybe (fail "unreadable dbversion file") pure (readMaybe (T.unpack (T.strip content)))

versionFile :: FilePath -> FilePath
versionFile root = root </> "dbversion"
