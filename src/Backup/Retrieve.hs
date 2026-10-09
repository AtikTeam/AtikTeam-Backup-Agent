-- | Retrieval of a snapshot from the backup server.
--
module Backup.Retrieve
  ( snapshotInstance
  , RetrieveError (..)
  ) where

import qualified Backup.DB as DB
import Backup.Env
import Backup.Time (getLocalTime)
import Backup.Types

import Control.Exception.Safe (Exception (..), throwIO, tryAny)
import Control.Monad (unless, when)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Traversable (for)
import qualified Network.HTTP.Client as H
import Network.HTTP.Types.Status (statusCode, statusMessage)
import System.IO (Handle)

-- Errors --------------------------------------------------------------------

-- | Why a snapshot could not be taken. The URLs never contain the
-- authentication key, which is sent in a cookie.
data RetrieveError
  = -- | The server answered with a status other than 200: URL, status code,
    -- reason phrase, start of the body.
    HttpStatusError Text Int Text Text
  | -- | The sitemap was received (200) but is not valid: URL, decoding error,
    -- start of the body.
    SiteMapDecodeError Text String Text
  | -- | An external file URL does not use HTTPS.
    NonHttpsUrl Text
  | -- | Some files could not be downloaded: failures, files to download.
    DownloadsFailed Int Int
  deriving (Show)

instance Exception RetrieveError where
  displayException = \case
    HttpStatusError url code reason body ->
      T.unpack $
        url <> ": HTTP " <> T.pack (show code) <> " " <> reason
          <> statusHint code
          <> bodyExcerpt body
    SiteMapDecodeError url err body ->
      T.unpack (url <> ": invalid sitemap: " <> T.pack err <> bodyExcerpt body)
    NonHttpsUrl url ->
      T.unpack ("refusing non-HTTPS URL: " <> url)
    DownloadsFailed failed total ->
      show failed <> " of " <> show total
        <> " file(s) could not be downloaded, snapshot abandoned"

-- | Explanation added to the message of some statuses.
statusHint :: Int -> Text
statusHint code
  | code == 401 || code == 403 =
      " (access refused: the backup identifier may have been revoked)"
  | code >= 500 = " (server error, the backup will be retried)"
  | otherwise = ""

-- | Start of the response body, in an error message.
bodyExcerpt :: Text -> Text
bodyExcerpt body
  | T.null body = ""
  | otherwise = " -- response starts with: " <> body

-- Snapshot ------------------------------------------------------------------

-- | Takes a new snapshot: downloads the sitemap, then the files that are not
-- yet in the database, and saves the sitemap last.
--
-- Atomicity: the sitemap is only saved if all the files were downloaded, so a
-- snapshot that exists is always complete. Files downloaded before a failure
-- are kept and not downloaded again at the next attempt.
--
-- Every failure is raised as an exception, to be logged by the caller.
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
  -- a failed file must not prevent the download of the others
  succeeded <- for missing $ \res ->
    tryAny (download env name conf res) >>= \case
      Right () -> pure True
      Left err -> do
        say LogError (resUrl res <> ": " <> T.pack (displayException err))
        pure False
  let failed = length (filter not succeeded)
  when (failed > 0) $
    throwIO (DownloadsFailed failed (length missing))
  DB.saveSiteMap root name started siteMap

-- | Downloads and decodes the current sitemap.
fetchSiteMap :: Env -> InstanceName -> InstanceConf -> IO SiteMap
fetchSiteMap env name conf = do
  -- the sitemap can be long to produce
  let req = (internalRequest conf name "/sitemap.json") {H.responseTimeout = timeoutSeconds 120}
  H.withResponse req (envManager env) $ \response -> do
    requireOk req response
    body <- BL.fromChunks <$> H.brConsume (H.responseBody response)
    case eitherDecode body of
      Right siteMap -> pure siteMap
      Left err -> throwIO (SiteMapDecodeError (requestUrl req) err (excerpt body))

-- | Downloads a file into the database.
--
-- The status is checked before anything is written: an error response
-- creates no file at all. The content is then written to a temporary file and
-- renamed (see 'DB.saveResource'), so that an interrupted download is never
-- mistaken for a complete file. A body shorter than its Content-Length raises
-- an exception (http-client).
download :: Env -> InstanceName -> InstanceConf -> Resource -> IO ()
download env name conf res = do
  logInstance env name LogInfo (resEtag res <> " <- " <> resUrl res)
  req <- case resSource res of
    Internal -> pure (internalRequest conf name (resUrl res))
    External -> externalRequest conf (resUrl res)
  H.withResponse req (envManager env) $ \response -> do
    requireOk req response
    DB.saveResource (envRoot env) name (resEtag res) (copyBody (H.responseBody response))

-- | Copies a response body to a handle, chunk by chunk, so that a large file
-- is never held in memory. An empty chunk marks the end of the body.
copyBody :: H.BodyReader -> Handle -> IO ()
copyBody body handle = do
  chunk <- H.brRead body
  unless (B.null chunk) $ do
    B.hPut handle chunk
    copyBody body handle

-- | Raises 'HttpStatusError' unless the status is 200 OK. Must be called
-- before the body is used. The start of the body is read for the message.
requireOk :: H.Request -> H.Response H.BodyReader -> IO ()
requireOk req response =
  unless (code == 200) $ do
    start <- H.brReadSome (H.responseBody response) excerptLength
    throwIO $
      HttpStatusError
        (requestUrl req)
        code
        (TE.decodeUtf8Lenient (statusMessage status))
        (excerpt start)
  where
    status = H.responseStatus response
    code = statusCode status

-- | Start of a body, as text on one line, for error messages.
excerpt :: BL.ByteString -> Text
excerpt =
  T.unwords . T.words . TE.decodeUtf8Lenient . BL.toStrict . BL.take (fromIntegral excerptLength)

excerptLength :: Int
excerptLength = 300

-- Requests ------------------------------------------------------------------

-- | Request to the backup server, with authentication. The path is relative to
-- the application.
internalRequest :: InstanceConf -> InstanceName -> Text -> H.Request
internalRequest conf name path =
  withSettings conf $
    H.defaultRequest
      { H.host = TE.encodeUtf8 (hostDomain host)
      , H.port = hostPort host
      , H.secure = True
      , H.path = TE.encodeUtf8 (hostPrefix host <> "/" <> name <> path)
      , H.requestHeaders = [("Cookie", TE.encodeUtf8 cookie)]
      }
  where
    host = confHost conf
    cookie = "backup_auth_key=" <> confAuthToken conf <> "; backup_protocol_version=2"

-- | Request to an external URL. It does not send the authentication and must
-- use HTTPS.
externalRequest :: InstanceConf -> Text -> IO H.Request
externalRequest conf url = do
  req <- H.parseRequest (T.unpack url)
  unless (H.secure req) $ throwIO (NonHttpsUrl url)
  pure (withSettings conf req)

-- | Settings shared by all requests.
withSettings :: InstanceConf -> H.Request -> H.Request
withSettings conf req =
  req
    { H.proxy = proxyOf conf
    , H.responseTimeout = timeoutSeconds 60
      -- the status is checked by 'requireOk', which also reads the start of
      -- the body for the error message
    , H.checkResponse = \_ _ -> pure ()
    }

-- | URL of a request, for messages. It never contains the authentication key
-- (sent in a cookie).
requestUrl :: H.Request -> Text
requestUrl = T.pack . show . H.getUri

proxyOf :: InstanceConf -> Maybe H.Proxy
proxyOf conf = (\p -> H.Proxy (TE.encodeUtf8 (proxyHost p)) (proxyPort p)) <$> confProxy conf

timeoutSeconds :: Int -> H.ResponseTimeout
timeoutSeconds s = H.responseTimeoutMicro (s * 1000000)
