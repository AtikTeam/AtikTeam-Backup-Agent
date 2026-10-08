-- | Command line.
module Backup.CLI
  ( Options (..)
  , parseOptions
  ) where

import Backup.Env (LogLevel (..), allEnum)
import Data.Char (toLower)
import Options.Applicative
import Text.Read (readMaybe)

data Options = Options
  { optPort :: Int
  , optHost :: String
  , optLogLevel :: LogLevel
  , optDataDir :: Maybe FilePath
  , optStaticDir :: Maybe FilePath
  , optInterval :: Int
  }

-- | Parses the command line. The version text is shown by @--version@.
parseOptions :: String -> IO Options
parseOptions version =
  execParser $
    info
      (options <**> simpleVersioner version <**> helper)
      (fullDesc <> progDesc desc)
  where
    desc="Local backup of AtikTeam data, browsable in a web browser. This server does not include access control. If you need it, you can use a reverse proxy in front to manage access control."


options :: Parser Options
options =
  Options
    <$> option
      (intIn 1 65535)
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 5500
          <> showDefault
          <> help "listening port of the HTTP server. Does not require admin privilege."
      )
    <*> strOption
      ( long "host"
          <> metavar "ADDRESS"
          <> value "127.0.0.1"
          <> showDefault
          <> help "listening address of the HTTP server (127.0.0.1: this computer only)"
      )
    <*> option
      (eitherReader parseLevel)
      ( long "log-level"
          <> short 'l'
          <> metavar "LEVEL"
          <> value LogNotice
          <> showDefaultWith levelName
          <> help "log level: info | notice | error"
      )
    <*> optional
      ( strOption
          ( long "data-dir"
              <> metavar "DIRECTORY"
              <> help "database directory (default: the AtikTeam directory in the user data directory)"
          )
      )
    <*> optional
      ( strOption
          ( long "static-dir"
              <> metavar "DIRECTORY"
              <> help "serve the static files (style sheet, script, logo) from this directory instead of the embedded ones, for interface development"
          )
      )
    <*> option
      (intIn 3600 42000)
      ( long "interval"
          <> metavar "SECONDS"
          <> value (4 * 60 * 60)
          <> showDefault
          <> help "delay between two backup cycles"
      )

intIn :: Int -> Int -> ReadM Int
intIn low high = eitherReader $ \s -> case readMaybe s of
  Just n | n >= low, n <= high -> Right n
  _ -> Left ("expected an integer between " <> show low <> " and " <> show high)

levelName :: LogLevel -> String
levelName = \case
  LogInfo -> "info"
  LogNotice -> "notice"
  LogError -> "error"


parseLevel :: String -> Either String LogLevel
parseLevel s = let level_map = map (\l-> (levelName l, l)) allEnum
  in case lookup (map toLower s) level_map of
       Nothing -> Left "unknown log level. Use: info | notice | error"
       Just level -> Right level
