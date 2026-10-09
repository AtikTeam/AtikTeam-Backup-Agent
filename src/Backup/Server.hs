-- | HTTP server: browsing of the backups and settings.
--
module Backup.Server
  ( Config (..)
  , run
  ) where

import qualified Backup.DB as DB
import Backup.Env
import Backup.Form
import Backup.Messages (Lang, parseLang)
import Backup.Static (lookupStatic)
import Backup.Time (getLocalTime, parseSnapshotName)
import Backup.Types
import qualified Backup.Views as Views
import Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (dayOfWeek, localDay)
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (doesFileExist)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

data Config = Config
  { cfgHost :: String
  , cfgPort :: Int
  , cfgStaticDir :: Maybe FilePath
  }

-- | Files of a snapshot, indexed by their path in the backed-up site.
type Files = Map.Map Text (FilePath, Http.ResponseHeaders)

data Server = Server
  { srvEnv :: Env
  , srvConfig :: Config
  , srvRequestBackup :: IO ()
  , srvRequestExit :: IO ()
  , srvCache :: IORef (Maybe ((InstanceName, SnapshotId), Files))
  }

type Handler = Wai.Request -> IO Wai.Response

-- | Runs the server. The two actions request an immediate backup cycle and the
-- shutdown of the program.
run :: Env -> Config -> IO () -> IO () -> IO ()
run env config requestBackup requestExit = do
  cache <- newIORef Nothing
  let server = Server env config requestBackup requestExit cache
      settings =
        Warp.setHost (fromString (cfgHost config)) $
          Warp.setPort (cfgPort config) Warp.defaultSettings
  Warp.runSettings settings $ sameOrigin $ \req respond ->
    route server req >>= respond

route :: Server -> Handler
route server req = case (Wai.requestMethod req, Wai.pathInfo req) of
  ("GET", []) -> rootPage server
  ("GET", ["_", file]) -> staticFile server file
  ("POST", ["_", "exit"]) -> exit server
  ("POST", ["_", "language"]) -> changeLanguage server req
  ("POST", ["_", "instance", "create"]) -> createInstance server req
  ("POST", ["_", "instance", "update", name]) -> updateInstance server name req
  ("GET", [name]) -> instancePage server name
  ("GET", name : snapshot : file) -> snapshotFile server name snapshot file
  _ -> notFound server

-- Handlers ------------------------------------------------------------------

rootPage :: Server -> IO Wai.Response
rootPage server = do
  names <- DB.listInstances (rootDir server)
  page server Http.status200 (`Views.rootPage` names)

staticFile :: Server -> Text -> IO Wai.Response
staticFile server file = do
  found <- lookupStatic (cfgStaticDir (srvConfig server)) file
  case found of
    Nothing -> notFound server
    Just (content, headers) -> pure (Wai.responseLBS Http.status200 headers (BL.fromStrict content))

instancePage :: Server -> Text -> IO Wai.Response
instancePage server nameText = withInstance server nameText $ \name -> do
  conf <- DB.loadInstance (rootDir server) name
  snapshots <- DB.listSnapshots (rootDir server) name
  page server Http.status200 $ \lang -> Views.instancePage lang name conf snapshots

snapshotFile :: Server -> Text -> Text -> [Text] -> IO Wai.Response
snapshotFile server nameText snapshotText segments = withInstance server nameText $ \name ->
  case parseSnapshotName (T.unpack snapshotText) of
    Nothing -> notFound server
    Just snapshot -> do
      files <- snapshotFiles server name snapshot
      case files >>= Map.lookup ("/" <> T.intercalate "/" segments) of
        Nothing -> notFound server
        Just (path, headers) -> do
          present <- doesFileExist path
          if present
            then pure (Wai.responseFile Http.status200 headers path Nothing)
            else notFound server

createInstance :: Server -> Handler
createInstance server req = withForm server req $ \form -> do
  today <- dayOfWeek . localDay <$> getLocalTime
  case newInstanceFromForm today form of
    Left err -> badRequest server err
    Right (name, conf) -> do
      DB.createInstance (rootDir server) name conf
      srvRequestBackup server
      pure (redirect (Views.pathTo [name]))

updateInstance :: Server -> Text -> Handler
updateInstance server nameText req = withInstance server nameText $ \name ->
  withForm server req $ \form -> do
    conf <- DB.loadInstance (rootDir server) name
    DB.saveInstance (rootDir server) name (updateConfFromForm form conf)
    srvRequestBackup server
    pure (redirect (Views.pathTo [name]))

changeLanguage :: Server -> Handler
changeLanguage server req = withForm server req $ \form ->
  case lookup "language" form >>= parseLang of
    Nothing -> badRequest server InvalidForm
    Just lang -> do
      DB.saveLanguage (rootDir server) lang
      pure (redirect "/")

exit :: Server -> IO Wai.Response
exit server = do
  srvRequestExit server
  page server Http.status200 Views.stopping

-- Snapshot index 

-- | Files of a snapshot, or 'Nothing' if that snapshot does not exist.
snapshotFiles :: Server -> InstanceName -> SnapshotId -> IO (Maybe Files)
snapshotFiles server name snapshot = do
  cached <- readIORef (srvCache server)
  case cached of
    Just (key, files) | key == (name, snapshot) -> pure (Just files)
    _ -> do
      known <- DB.listSnapshots (rootDir server) name
      if snapshot `notElem` known
        then pure Nothing
        else do
          siteMap <- DB.loadSiteMap (rootDir server) name snapshot
          let files = Map.fromList (map entry siteMap)
          writeIORef (srvCache server) (Just ((name, snapshot), files))
          pure (Just files)
  where
    entry res =
      ( resPath res
      , (DB.resourcePath (rootDir server) name (resEtag res), toHeaders (resHeaders res))
      )
    toHeaders = map (bimap (CI.mk . TE.encodeUtf8) TE.encodeUtf8)

-- Utilities 

rootDir :: Server -> FilePath
rootDir = envRoot . srvEnv

-- | Runs the action if the application exists. The name comes from the URL, so
-- it is only used once found among the applications of the database.
withInstance :: Server -> Text -> (InstanceName -> IO Wai.Response) -> IO Wai.Response
withInstance server nameText action = do
  names <- DB.listInstances (rootDir server)
  maybe (notFound server) action (find (== nameText) names)

-- | Decodes a form sent with POST (size limited to 64 KiB).
withForm :: Server -> Wai.Request -> (Form -> IO Wai.Response) -> IO Wai.Response
withForm server req action = case Wai.requestBodyLength req of
  Wai.KnownLength size | size <= 65536 -> do
    body <- Wai.strictRequestBody req
    action (decode (BL.toStrict body))
  _ -> badRequest server InvalidForm
  where
    decode = map (bimap TE.decodeUtf8Lenient TE.decodeUtf8Lenient) . Http.parseSimpleQuery

-- | Rejects POST requests sent from another origin (CSRF): without this, any
-- web page open in the browser could change the settings or stop the program.
sameOrigin :: Wai.Middleware
sameOrigin app req respond
  | Wai.requestMethod req == "POST"
  , Just origin <- lookup "Origin" (Wai.requestHeaders req)
  , Just host <- Wai.requestHeaderHost req
  , not (("//" <> host) `B.isSuffixOf` origin) =
      respond (Wai.responseLBS Http.status403 [] "Forbidden origin")
  | otherwise = app req respond

-- | An HTML page in the language chosen by the user.
page :: Server -> Http.Status -> (Lang -> Html) -> IO Wai.Response
page server status view = do
  lang <- DB.loadLanguage (rootDir server)
  pure $
    Wai.responseBuilder
      status
      [(Http.hContentType, "text/html; charset=UTF-8")]
      (renderHtmlBuilder (view lang))

notFound :: Server -> IO Wai.Response
notFound server = page server Http.status404 Views.notFound

badRequest :: Server -> FormError -> IO Wai.Response
badRequest server err = page server Http.status400 (`Views.errorPage` err)

redirect :: Text -> Wai.Response
redirect target =
  Wai.responseLBS Http.status303 [(Http.hLocation, TE.encodeUtf8 target)] "See other"
