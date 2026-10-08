-- | Local database, stored in plain files.
--
-- > root /
-- >   dbversion
-- >   language                    (UI language code, see "Backup.Messages")
-- >   instances /
-- >     APPLICATION-NAME /
-- >       config                  (JSON, see 'InstanceConf')
-- >       resources /  ETAG ...   (content of the backed-up files)
-- >       sitemaps /   DATE ...   (JSON, one per snapshot, see "Backup.Time")
--
-- Every function takes the database root as its first argument and throws an
-- IO exception on failure (missing file, unreadable JSON).
module Backup.DB
  ( getSystemDbRootDir
  , instancesDir
  , listInstances
  , createInstance
  , loadInstance
  , saveInstance
  , listSnapshots
  , loadSiteMap
  , saveSiteMap
  , deleteSnapshot
  , resourcePath
  , listResources
  , garbageCollect
  , loadLanguage
  , saveLanguage
  , saveResource
  ) where

import Backup.Messages (Lang (..), langCode, parseLang)
import Backup.Time (parseSnapshotName, snapshotName)
import Backup.Types
import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encodeFile)
import qualified Data.ByteString as B
import Data.Foldable (traverse_)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getAppUserDataDirectory
  , listDirectory
  , removeFile
  , renameFile
  )
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), Handle, withBinaryFile)

-- | Default location of the database
getSystemDbRootDir :: IO FilePath
getSystemDbRootDir = (</> "backup") <$> getAppUserDataDirectory "AtikTeam"

-- Paths ---------------------------------------------------------------------

instancesDir :: FilePath -> FilePath
instancesDir root = root </> "instances"

instanceDir :: FilePath -> InstanceName -> FilePath
instanceDir root name = instancesDir root </> T.unpack name

sitemapsDir :: FilePath -> InstanceName -> FilePath
sitemapsDir root name = instanceDir root name </> "sitemaps"

resourcesDir :: FilePath -> InstanceName -> FilePath
resourcesDir root name = instanceDir root name </> "resources"

configFile :: FilePath -> InstanceName -> FilePath
configFile root name = instanceDir root name </> "config"

sitemapFile :: FilePath -> InstanceName -> SnapshotId -> FilePath
sitemapFile root name snapshot = sitemapsDir root name </> snapshotName snapshot

resourcePath :: FilePath -> InstanceName -> Etag -> FilePath
resourcePath root name etag = resourcesDir root name </> T.unpack etag

languageFile :: FilePath -> FilePath
languageFile root = root </> "language"

-- Applications --------------------------------------------------------------

listInstances :: FilePath -> IO [InstanceName]
listInstances root = do
  entries <- listDirectory (instancesDir root)
  dirs <- filterM (doesDirectoryExist . (instancesDir root </>)) entries
  pure (map T.pack (sort dirs))

createInstance :: FilePath -> InstanceName -> InstanceConf -> IO ()
createInstance root name conf = do
  createDirectoryIfMissing True (sitemapsDir root name)
  createDirectoryIfMissing True (resourcesDir root name)
  saveInstance root name conf

loadInstance :: FilePath -> InstanceName -> IO InstanceConf
loadInstance root name = readJson (configFile root name)

saveInstance :: FilePath -> InstanceName -> InstanceConf -> IO ()
saveInstance root name = writeJson (configFile root name)

-- Snapshots -----------------------------------------------------------------

-- | Snapshots of an application, most recent first. Files whose name is not a
-- snapshot name are ignored.
listSnapshots :: FilePath -> InstanceName -> IO [SnapshotId]
listSnapshots root name =
  sortOn Down . mapMaybe parseSnapshotName <$> listDirectory (sitemapsDir root name)

loadSiteMap :: FilePath -> InstanceName -> SnapshotId -> IO SiteMap
loadSiteMap root name snapshot = readJson (sitemapFile root name snapshot)

saveSiteMap :: FilePath -> InstanceName -> SnapshotId -> SiteMap -> IO ()
saveSiteMap root name snapshot = writeJson (sitemapFile root name snapshot)

deleteSnapshot :: FilePath -> InstanceName -> SnapshotId -> IO ()
deleteSnapshot root name snapshot = removeFile (sitemapFile root name snapshot)

-- Resources -----------------------------------------------------------------

-- | Etags of the files present in the database.
listResources :: FilePath -> InstanceName -> IO (Set.Set Etag)
listResources root name =
  Set.fromList . map T.pack <$> listDirectory (resourcesDir root name)

-- | Garbage collector: removes the files of @resources@ that no snapshot
-- refers to (including interrupted downloads). Returns the removed files.
--
-- If a sitemap cannot be read, an exception is thrown before anything is
-- removed: it is better to keep useless files than to lose useful ones.
garbageCollect :: FilePath -> InstanceName -> IO [FilePath]
garbageCollect root name = do
  snapshots <- listSnapshots root name
  siteMaps <- traverse (loadSiteMap root name) snapshots
  present <- listResources root name
  let referenced = Set.fromList [resEtag r | siteMap <- siteMaps, r <- siteMap]
  let garbage = Set.toList $ Set.difference present referenced
  traverse_ (removeFile . resourcePath root name) garbage
  pure (map T.unpack garbage)


-- Settings ------------------------------------------------------------------

-- | Language of the web interface: English unless another one was chosen.
loadLanguage :: FilePath -> IO Lang
loadLanguage root = do
  exists <- doesFileExist (languageFile root)
  if exists
    then fromMaybe English . parseLang . T.strip . TE.decodeUtf8Lenient <$>
         B.readFile (languageFile root)
    else pure English

saveLanguage :: FilePath -> Lang -> IO ()
saveLanguage root lang =
  writeAtomic (languageFile root) (\tmp -> B.writeFile tmp (TE.encodeUtf8 (langCode lang)))

-- Files ---------------------------------------------------------------------

readJson :: FromJSON a => FilePath -> IO a
readJson path =
  eitherDecodeFileStrict' path >>= either (\err -> fail (path <> ": " <> err)) pure

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson path value = writeAtomic path (`encodeFile` value)

-- | Stores a resource: the content is written by the given action to a
-- temporary file, then renamed.
saveResource :: FilePath -> InstanceName -> Etag -> (Handle -> IO ()) -> IO ()
saveResource root name etag write =
  writeAtomic (resourcePath root name etag) (\tmp -> withBinaryFile tmp WriteMode write)

-- | Writes to a temporary file, then renames it, so that an interruption never
-- leaves a truncated file.
writeAtomic :: FilePath -> (FilePath -> IO ()) -> IO ()
writeAtomic path write = do
  let tmp = path <> ".tmp"
  write tmp
  renameFile tmp path
