-- | Interpretation of HTML forms (pure functions).
module Backup.Form
  ( Form
  , FormError (..)
  , newInstanceFromForm
  , updateConfFromForm
  ) where

import Backup.Types
import Control.Monad (mfilter, unless, when)
import Data.List (nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (DayOfWeek)
import Text.Read (readMaybe)

-- | Fields of a form, already decoded.
type Form = [(Text, Text)]

-- | Why a form was rejected. The web interface turns these into messages in
-- the language of the user.
data FormError
  = MissingToken
  | MalformedToken
  | InvalidName
  | InvalidForm
  deriving (Eq, Show)

-- | Creates an application from the @name/key@ backup identifier provided by
-- AtikTeam. The first backup is scheduled on the given day (in practice,
-- today).
newInstanceFromForm :: DayOfWeek -> Form -> Either FormError (InstanceName, InstanceConf)
newInstanceFromForm today form = do
  token <- maybe (Left MissingToken) Right (mfilter (not . T.null) (T.strip <$> lookup "full-token" form))
  let (name, rest) = T.breakOn "/" token
      authKey = T.strip (T.drop 1 rest)
  unless (isValidInstanceName name) $ Left InvalidName
  when (T.null authKey) $ Left MalformedToken
  pure (name, defaultConf {confAuthToken = authKey, confBackupDays = [today]})

-- | Applies the settings form. An invalid history size keeps the previous
-- value; an incomplete or invalid proxy removes the proxy setting.
updateConfFromForm :: Form -> InstanceConf -> InstanceConf
updateConfFromForm form conf =
  conf
    { confBackupDays = nub (mapMaybe parseDay [day | ("backup-day", day) <- form])
    , confHistorySize = fromMaybe (confHistorySize conf) (field "history-size" >>= positiveInt)
    , confProxy =
        ProxyConf
          <$> mfilter (not . T.null) (field "proxy-host")
          <*> (field "proxy-port" >>= portNumber)
    }
  where
    field key = T.strip <$> lookup key form

positiveInt :: Text -> Maybe Int
positiveInt = mfilter (>= 1) . readMaybe . T.unpack

portNumber :: Text -> Maybe Int
portNumber = mfilter (\p -> p >= 1 && p <= 65535) . readMaybe . T.unpack

defaultConf :: InstanceConf
defaultConf =
  InstanceConf
    { confHost = BackupHost {hostDomain = "backup.atikteam.com", hostPort = 443, hostPrefix = ""}
    , confProxy = Nothing
    , confAuthToken = ""
    , confBackupDays = []
    , confHistorySize = 5
    }
