-- | HTML pages of the web interface. Translations come from
-- "Backup.Messages"
module Backup.Views
  ( rootPage
  , instancePage
  , notFound
  , errorPage
  , stopping
  , pathTo
  ) where

import Backup.Form (FormError (..))
import Backup.Messages
import Backup.Time (snapshotName)
import Backup.Types
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (urlEncode)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | URL path made of the given segments, each one URL-encoded.
pathTo :: [Text] -> Text
pathTo segments = "/" <> T.intercalate "/" (map encode segments)
  where
    encode = TE.decodeUtf8 . urlEncode False . TE.encodeUtf8

-- Pages 

rootPage :: Lang -> [InstanceName] -> Html
rootPage lang names = layout lang $ do
  H.h1 (msg lang MsgApplications)
  if null names
    then H.p (msg lang MsgNoApplication)
    else H.ul ! A.class_ "items" $ forM_ names $ \name ->
      H.li $ H.a ! A.href (link [name]) $ H.toHtml name
  H.h2 (msg lang MsgAddApplication)
  H.form ! A.action "/_/instance/create" ! A.method "POST" $ do
    H.div ! A.class_ "field" $ do
      H.label ! A.for "full-token" $ msg lang MsgTokenLabel
      H.input ! A.id "full-token" ! A.type_ "text" ! A.name "full-token" ! A.required "required"
      H.p ! A.class_ "help" $ msg lang MsgTokenHelp
    H.button ! A.class_ "primary" ! A.type_ "submit" $ msg lang MsgAddApplicationButton

-- | Page of an application: its snapshots (dated in local time) and the
-- settings form.
instancePage :: Lang -> InstanceName -> InstanceConf -> [SnapshotId] -> Html
instancePage lang name conf snapshots = layout lang $ do
  H.h1 (msg lang (MsgBackupsOf name))
  if null snapshots
    then H.p (msg lang MsgNoBackup)
    else H.ul ! A.class_ "items" $ forM_ snapshots $ \snapshot ->
      H.li $
        H.a ! A.href (link [name, T.pack (snapshotName snapshot), "index.html"]) $
          msg lang (MsgSnapshotDate snapshot)
  H.details ! A.class_ "settings" $ do
    H.summary (msg lang MsgEditSettings)
    H.form ! A.action (link ["_", "instance", "update", name]) ! A.method "POST" $ do
      H.fieldset ! A.class_ "field" $ do
        H.legend (msg lang MsgBackupDaysLabel)
        H.div ! A.class_ "days" $ forM_ allDays dayCheckbox
        H.p ! A.class_ "help" $ msg lang MsgBackupDaysHelp
      H.div ! A.class_ "field" $ do
        H.label ! A.for "history-size" $ msg lang MsgHistorySizeLabel
        H.input
          ! A.id "history-size"
          ! A.type_ "number"
          ! A.name "history-size"
          ! A.min "1"
          ! A.value (H.toValue (confHistorySize conf))
        H.p ! A.class_ "help" $ msg lang MsgHistorySizeHelp
      H.div ! A.class_ "field" $ do
        H.label ! A.for "proxy-host" $ msg lang MsgProxyHostLabel
        H.input
          ! A.id "proxy-host"
          ! A.type_ "text"
          ! A.name "proxy-host"
          ! A.value (H.toValue (maybe "" proxyHost (confProxy conf)))
      H.div ! A.class_ "field" $ do
        H.label ! A.for "proxy-port" $ msg lang MsgProxyPortLabel
        H.input
          ! A.id "proxy-port"
          ! A.type_ "number"
          ! A.name "proxy-port"
          ! A.min "1"
          ! A.max "65535"
          ! A.value (H.toValue (maybe "" (show . proxyPort) (confProxy conf)))
        H.p ! A.class_ "help" $ msg lang MsgProxyHelp
      H.button ! A.class_ "primary" ! A.type_ "submit" $ msg lang MsgSaveChanges
  where
    dayCheckbox day =
      H.label $ do
        H.input
          ! A.type_ "checkbox"
          ! A.name "backup-day"
          ! A.value (H.toValue (show day))
          ! (if day `elem` confBackupDays conf then A.checked "checked" else mempty)
        msg lang (MsgDay day)

notFound :: Lang -> Html
notFound lang = layout lang $ do
  H.h1 (msg lang MsgNotFoundTitle)
  H.p $ do
    msg lang MsgNotFoundText
    " "
    backHome lang

errorPage :: Lang -> FormError -> Html
errorPage lang err = layout lang $ do
  H.h1 (msg lang MsgInvalidRequestTitle)
  H.p $ do
    msg lang (errorMessage err)
    " "
    backHome lang

stopping :: Lang -> Html
stopping lang = layout lang $ H.h1 (msg lang MsgStopping)

-- Layout

layout :: Lang -> Html -> Html
layout lang content = H.docTypeHtml ! A.lang (H.toValue (langCode lang)) $ do
  H.head $ do
    H.meta ! A.charset "UTF-8"
    H.title (msg lang MsgSiteTitle)
    H.link ! A.rel "stylesheet" ! A.href "/_/stylesheet.css"
    H.script ! A.src "/_/javascript.js" ! A.defer "defer" $ mempty
  H.body $ do
    H.header $ do
      H.a ! A.class_ "logo" ! A.href "/" $ H.img ! A.src "/_/atikteam.png" ! A.alt "AtikTeam"
      H.a ! A.class_ "title" ! A.href "/" $ msg lang MsgSiteTitle
      languageSelector lang
    H.main content
    H.footer $ H.a ! A.href "http://www.atikteam.com" $ "AtikTeam ©"

-- | One button per language; the current one is disabled.
languageSelector :: Lang -> Html
languageSelector current =
  H.form ! A.class_ "language" ! A.action "/_/language" ! A.method "POST" $
    forM_ [minBound .. maxBound] $ \lang ->
      H.button
        ! A.type_ "submit"
        ! A.name "language"
        ! A.value (H.toValue (langCode lang))
        ! (if lang == current then A.disabled "disabled" else mempty)
        $ H.toHtml (langName lang)

-- Utilities 

msg :: Lang -> Msg -> Html
msg lang = H.toHtml . render lang

errorMessage :: FormError -> Msg
errorMessage = \case
  MissingToken -> MsgErrMissingToken
  MalformedToken -> MsgErrMalformedToken
  InvalidName -> MsgErrInvalidName
  InvalidForm -> MsgErrInvalidForm

backHome :: Lang -> Html
backHome lang = H.a ! A.href "/" $ msg lang MsgBackHome

link :: [Text] -> H.AttributeValue
link = H.toValue . pathTo
