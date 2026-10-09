-- | Retrieval of a snapshot from the backup server.
module Backup.Retrieve (snapshotInstance) where

import qualified Backup.DB as DB
import Backup.Env
import Backup.Time (getLocalTime)
import Backup.Types

import qualified Data.Set as Set
import Control.Exception (displayException)
import Control.Exception.Safe (catchAny)
import Control.Monad (unless, when)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Traversable (for)
import qualified Network.HTTP.Client as H
import System.IO (Handle)

-- | Takes a new snapshot: downloads the sitemap, then the files that are not
-- yet in the database, and saves the sitemap last.
--
-- Atomicity : the sitemap is only saved if all the files were
-- downloaded: a snapshot that exists is therefore always complete.
--
snapshotInstance :: Env -> InstanceName -> InstanceConf -> IO ()
snapshotInstance env name conf = do
  let root = envRoot env
      say = logInstance env name
  started <- getLocalTime
  say LogInfo "retrieving the sitemap"
  siteMap <- fetchSiteMap env name conf
  present <- DB.listResources root name
  let wanted = Set.fromList (map resEtag siteMap)
      toGet = Set.difference wanted present
      isMissing res = Set.member (resEtag res) toGet
      missing = nubOrdOn resEtag (filter isMissing siteMap)
  say LogInfo (T.pack (show (length missing)) <> " file(s) to download")
  failures <- for missing $ \res ->
    (download env name conf res >> pure False) `catchAny` \err -> do
      say LogError (resUrl res <> ": " <> T.pack (displayException err))
      pure True
  let failed = length (filter id failures)
  when (failed > 0) $
    fail (show failed <> " file(s) could not be downloaded, snapshot abandoned")
  DB.saveSiteMap root name started siteMap


-- | Downloads and decodes the current sitemap.
fetchSiteMap :: Env -> InstanceName -> InstanceConf -> IO SiteMap
fetchSiteMap env name conf = do
  -- the response can be particularly long
  let req = (internalRequest conf name "/sitemap.json") {H.responseTimeout = timeoutSeconds 120}
  response <- H.httpLbs req (envManager env)
  case eitherDecode (H.responseBody response) of
    Right siteMap -> pure siteMap
    Left err -> do
      let start = TE.decodeUtf8Lenient (BL.toStrict (BL.take 1000 (H.responseBody response)))
      logInstance env name LogInfo ("start of the response: " <> start)
      fail ("unreadable sitemap: " <> err)

-- | Downloads a file into the database. The content is first written to a
-- temporary file, so that an interrupted download is never mistaken for a
-- complete file.
download :: Env -> InstanceName -> InstanceConf -> Resource -> IO ()
download env name conf res = do
  logInstance env name LogInfo (resEtag res <> " <- " <> resUrl res)
  req <- case resSource res of
    Internal -> pure (internalRequest conf name (resUrl res))
    External -> externalRequest conf (resUrl res)
  H.withResponse req (envManager env) $ \response ->
    DB.saveResource (envRoot env) name (resEtag res)
                    (copyBody (H.responseBody response))


-- | Copies a response body to a handle, chunk by chunk, so that a large file
-- is never held in memory. An empty chunk marks the end of the body.
copyBody :: H.BodyReader -> Handle -> IO ()
copyBody body handle = do
  chunk <- H.brRead body
  unless (B.null chunk) $ do
    B.hPut handle chunk
    copyBody body handle

-- Requests ------------------------------------------------------------------

-- | Request to the backup server, with authentication. The path is relative to
-- the application.
internalRequest :: InstanceConf -> InstanceName -> T.Text -> H.Request
internalRequest conf name path =
  H.defaultRequest
    { H.host = TE.encodeUtf8 (hostDomain host)
    , H.port = hostPort host
    , H.secure = True
    , H.path = TE.encodeUtf8 (hostPrefix host <> "/" <> name <> path)
    , H.requestHeaders = [("Cookie", TE.encodeUtf8 cookie)]
    , H.proxy = proxyOf conf
    , H.responseTimeout = timeoutSeconds 60
    }
  where
    host = confHost conf
    cookie = "backup_auth_key=" <> confAuthToken conf <> "; backup_protocol_version=2"

-- | Request to an external URL. It does not send the authentication and must
-- use HTTPS.
externalRequest :: InstanceConf -> T.Text -> IO H.Request
externalRequest conf url = do
  req <- H.parseRequest (T.unpack url)
  unless (H.secure req) $ fail ("refusing non-HTTPS URL: " <> T.unpack url)
  pure req {H.proxy = proxyOf conf, H.responseTimeout = timeoutSeconds 60}

proxyOf :: InstanceConf -> Maybe H.Proxy
proxyOf conf = (\p -> H.Proxy (TE.encodeUtf8 (proxyHost p)) (proxyPort p)) <$> confProxy conf

timeoutSeconds :: Int -> H.ResponseTimeout
timeoutSeconds s = H.responseTimeoutMicro (s * 1000000)
