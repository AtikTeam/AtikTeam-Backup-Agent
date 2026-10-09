-- | Texts of the web interface, in every supported language.
--
-- Each text is a constructor of 'Msg', and each language is a function from
-- 'Msg' to 'Text'. 
module Backup.Messages
  ( Lang (..)
  , langCode
  , langName
  , parseLang
  , Msg (..)
  , render
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (DayOfWeek (..), LocalTime, defaultTimeLocale, formatTime)

data Lang = English | French
  deriving (Eq, Show, Enum, Bounded)

-- | stored as a file in the database and used in forms and in the
-- HTML @lang@ attribute.
langCode :: Lang -> Text
langCode = \case
  English -> "en"
  French -> "fr"

-- | Name of a language in that language (shown in the language selector).
langName :: Lang -> Text
langName = \case
  English -> "En"
  French -> "Fr"

parseLang :: Text -> Maybe Lang
parseLang code = lookup code [(langCode lang, lang) | lang <- [minBound .. maxBound]]

data Msg
  = MsgSiteTitle
  | MsgApplications
  | MsgNoApplication
  | MsgAddApplication
  | MsgTokenHelp
  | MsgTokenLabel
  | MsgAddApplicationButton
  | MsgBackupsOf Text
  | MsgNoBackup
  | MsgSnapshotDate LocalTime
  | MsgEditSettings
  | MsgBackupDaysHelp
  | MsgBackupDaysLabel
  | MsgDay DayOfWeek
  | MsgHistorySizeHelp
  | MsgHistorySizeLabel
  | MsgProxyHelp
  | MsgProxyHostLabel
  | MsgProxyPortLabel
  | MsgSaveChanges
  | MsgNotFoundTitle
  | MsgNotFoundText
  | MsgBackHome
  | MsgInvalidRequestTitle
  | MsgErrMissingToken
  | MsgErrMalformedToken
  | MsgErrInvalidName
  | MsgErrInvalidForm
  | MsgStopping
  deriving (Eq, Show)

render :: Lang -> Msg -> Text
render = \case
  English -> english
  French -> french

english :: Msg -> Text
english = \case
  MsgSiteTitle -> "Backup"
  MsgApplications -> "Applications"
  MsgNoApplication -> "No application yet"
  MsgAddApplication -> "Add an application"
  MsgTokenHelp -> "The backup identifier to paste below can be obtained from the administration of your AtikTeam application."
  MsgTokenLabel -> "Backup identifier:"
  MsgAddApplicationButton -> "Add this application"
  MsgBackupsOf name -> "Backups of " <> name
  MsgNoBackup -> "No backup has been completed yet, please come back later"
  MsgSnapshotDate time -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %R" time)
  MsgEditSettings -> "Edit settings"
  MsgBackupDaysHelp -> "A backup is started automatically on the selected days, provided the backup program is running. The time of the backup is chosen automatically."
  MsgBackupDaysLabel -> "Backup days:"
  MsgDay day -> case day of
    Monday -> "Monday"
    Tuesday -> "Tuesday"
    Wednesday -> "Wednesday"
    Thursday -> "Thursday"
    Friday -> "Friday"
    Saturday -> "Saturday"
    Sunday -> "Sunday"
  MsgHistorySizeHelp -> "The history size is the number of successful backups to keep. When it is exceeded, the oldest backups are deleted automatically. Thanks to the smart storage mechanism, keeping several backups does not multiply disk usage."
  MsgHistorySizeLabel -> "History size:"
  MsgProxyHelp -> "Details of an optional proxy server (HTTPS PROXY). Leave the fields empty if you do not use a proxy server."
  MsgProxyHostLabel -> "Proxy server address (optional):"
  MsgProxyPortLabel -> "Proxy server port (optional):"
  MsgSaveChanges -> "Save changes"
  MsgNotFoundTitle -> "Page not found"
  MsgNotFoundText -> "The requested page does not exist."
  MsgBackHome -> "back to the home page"
  MsgInvalidRequestTitle -> "Invalid request"
  MsgErrMissingToken -> "The backup identifier is missing."
  MsgErrMalformedToken -> "The backup identifier is incomplete (expected “name/key”)."
  MsgErrInvalidName -> "Invalid application name (letters, digits, “.”, “-” and “_” only)."
  MsgErrInvalidForm -> "Invalid form."
  MsgStopping -> "Shutting down the backup system ..."

french :: Msg -> Text
french = \case
  MsgSiteTitle -> "Sauvegardes"
  MsgApplications -> "Applications"
  MsgNoApplication -> "aucune application pour le moment"
  MsgAddApplication -> "Ajouter une application"
  MsgTokenHelp -> "L'identifiant de sauvegarde à coller ci-dessous peut être obtenu depuis l’administration de votre application AtikTeam."
  MsgTokenLabel -> "Identifiant de sauvegarde :"
  MsgAddApplicationButton -> "Ajouter cette application"
  MsgBackupsOf name -> "Sauvegardes de " <> name
  MsgNoBackup -> "aucune sauvegarde n’a pu être terminée pour le moment, merci de repasser plus tard"
  MsgSnapshotDate time -> T.pack (formatTime defaultTimeLocale "le %d-%m-%Y à %R" time)
  MsgEditSettings -> "Régler les paramètres"
  MsgBackupDaysHelp -> "Une sauvegarde sera déclenchée automatiquement les jours choisis, à condition que le programme de sauvegarde soit en fonctionnement. L’heure de la sauvegarde sera déterminée automatiquement."
  MsgBackupDaysLabel -> "Jours de sauvegarde :"
  MsgDay day -> case day of
    Monday -> "Lundi"
    Tuesday -> "Mardi"
    Wednesday -> "Mercredi"
    Thursday -> "Jeudi"
    Friday -> "Vendredi"
    Saturday -> "Samedi"
    Sunday -> "Dimanche"
  MsgHistorySizeHelp -> "La taille de l’historique détermine le nombre de sauvegardes réussies à conserver. Lorsque cette taille est dépassée, les sauvegardes les plus anciennes sont automatiquement supprimées. Grâce au mécanisme de stockage intelligent, la conservation de plusieurs sauvegardes ne multiplie pas l’occupation du disque."
  MsgHistorySizeLabel -> "Taille de l’historique :"
  MsgProxyHelp -> "Informations sur un éventuel serveur mandataire (PROXY HTTPS). Laisser les champs vides si vous n’utilisez pas de serveur mandataire."
  MsgProxyHostLabel -> "Adresse du serveur mandataire (optionnel) :"
  MsgProxyPortLabel -> "Port du serveur mandataire (optionnel) :"
  MsgSaveChanges -> "Enregistrer ces modifications"
  MsgNotFoundTitle -> "Adresse introuvable"
  MsgNotFoundText -> "La page demandée n’existe pas."
  MsgBackHome -> "retour à l’accueil"
  MsgInvalidRequestTitle -> "Requête invalide"
  MsgErrMissingToken -> "L’identifiant de sauvegarde est manquant."
  MsgErrMalformedToken -> "L’identifiant de sauvegarde est incomplet (« nom/clé » attendu)."
  MsgErrInvalidName -> "Nom d’application invalide (lettres, chiffres, « . », « - » et « _ » uniquement)."
  MsgErrInvalidForm -> "Formulaire invalide."
  MsgStopping -> "Arrêt du système de sauvegarde en cours ..."
