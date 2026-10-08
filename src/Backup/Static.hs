{-# LANGUAGE TemplateHaskell #-}

-- | Static files of the web interface (style sheet, script, logo).
--
-- Files are embedded in the executable at compile time. For interface
-- development, a directory can be given instead: the same files are then read
-- from it on every request, so that changes show up without recompiling.
--
-- Only the files listed in 'staticFiles' are ever served, in both modes.
module Backup.Static (lookupStatic) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.FileEmbed (embedFile)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Types as Http
import System.FilePath ((</>))

data StaticFile = StaticFile
  { fileName :: FilePath
  -- ^ name in the URL and in the static directory
  , fileType :: ByteString
  -- ^ MIME type
  , fileEmbedded :: ByteString
  -- ^ content embedded at compile time
  }

-- | The static files that can be served. To add one, add it here.
staticFiles :: [StaticFile]
staticFiles =
  [ StaticFile "stylesheet.css" "text/css; charset=UTF-8" $(embedFile "static/stylesheet.css")
  , StaticFile "javascript.js" "text/javascript; charset=UTF-8" $(embedFile "static/javascript.js")
  , StaticFile "atikteam.png" "image/png" $(embedFile "static/atikteam.png")
  ]

-- | Content and response headers of a static file, if that name is one of
-- 'staticFiles'. With a directory, the file is read from it (and the browser
-- is told not to cache it); otherwise the embedded content is returned.
lookupStatic :: Maybe FilePath -> Text -> IO (Maybe (ByteString, Http.ResponseHeaders))
lookupStatic directory name =
  case find ((== T.unpack name) . fileName) staticFiles of
    Nothing -> pure Nothing
    Just file -> case directory of
      Nothing -> pure (Just (fileEmbedded file, typeHeader file))
      Just dir -> do
        content <- B.readFile (dir </> fileName file)
        pure (Just (content, (Http.hCacheControl, "no-cache") : typeHeader file))
  where
    typeHeader file = [(Http.hContentType, fileType file)]
