{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Yaptape.YouTube.Internal
  ( YouTubeVideoId (..)
  , YouTubeVideoIdError (..)
  , YouTubeUrlSpec (..)
  , youTubeUrlSpec
  , mkYouTubeVideoId
  , parseYouTubeUrl
  , unsafeMkYouTubeVideoId
  , renderYouTubeVideoIdError
  ) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.URI (URI (..), URIAuth (..), parseURI)
import Servant.API (FromHttpApiData (..), ToHttpApiData)

-- | Newtype representing a validated YouTube video ID.
-- The data constructor is internal so external modules cannot construct
-- unvalidated values.
newtype YouTubeVideoId = YouTubeVideoId { unYouTubeVideoId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, ToHttpApiData)

-- | Unsafely construct a 'YouTubeVideoId' without validation.
-- Intended for test fixtures or internal database mappers with trusted data.
unsafeMkYouTubeVideoId :: Text -> YouTubeVideoId
unsafeMkYouTubeVideoId = YouTubeVideoId

-- | Errors encountered when attempting to parse or validate a YouTube video ID.
data YouTubeVideoIdError
  = EmptyInput
  | InvalidLength Int
  | InvalidCharacters Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert a 'YouTubeVideoIdError' to a user-friendly error message.
renderYouTubeVideoIdError :: YouTubeVideoIdError -> Text
renderYouTubeVideoIdError EmptyInput = "YouTube video ID cannot be empty"
renderYouTubeVideoIdError (InvalidLength len) =
  "Invalid YouTube video ID length: expected 11 characters, got " <> T.pack (show len)
renderYouTubeVideoIdError (InvalidCharacters chars) =
  "Invalid characters in YouTube video ID: " <> chars

-- | Canonical specification of accepted YouTube URL structures and video ID format,
-- shared between server-side Haskell validation and client-side browser scripts.
data YouTubeUrlSpec = YouTubeUrlSpec
  { specSchemes      :: [Text]
  , specPrimaryHosts :: [Text]
  , specShortHosts   :: [Text]
  , specWatchPath    :: Text
  , specWatchParam   :: Text
  , specPathPrefixes :: [Text]
  , specIdLength     :: Int
  , specIdPattern    :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

youTubeUrlSpec :: YouTubeUrlSpec
youTubeUrlSpec = YouTubeUrlSpec
  { specSchemes      = ["https:", "http:"]
  , specPrimaryHosts = ["youtube.com", "www.youtube.com", "m.youtube.com", "music.youtube.com"]
  , specShortHosts   = ["youtu.be", "www.youtu.be"]
  , specWatchPath    = "watch"
  , specWatchParam   = "v"
  , specPathPrefixes = ["shorts", "embed", "live"]
  , specIdLength     = 11
  , specIdPattern    = "^[A-Za-z0-9_-]{11}$"
  }

-- | Validate candidate characters and length.
-- YouTube video IDs are strictly 11 base64url ASCII characters: [a-zA-Z0-9_-].
validateCandidate :: Text -> Either YouTubeVideoIdError YouTubeVideoId
validateCandidate cand
  | T.null cand = Left EmptyInput
  | T.length cand /= youTubeUrlSpec.specIdLength = Left (InvalidLength (T.length cand))
  | not (T.null invalidChars) = Left (InvalidCharacters invalidChars)
  | otherwise = Right (YouTubeVideoId cand)
  where
    isBase64UrlChar c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c == '_' ||
      c == '-'
    invalidChars = T.filter (not . isBase64UrlChar) cand

-- | Smart constructor for 'YouTubeVideoId'.
-- Trims surrounding whitespace and validates length & base64url characters.
mkYouTubeVideoId :: Text -> Either YouTubeVideoIdError YouTubeVideoId
mkYouTubeVideoId = validateCandidate . T.strip

-- | Parse a YouTube URL (youtube.com/watch, /shorts/, /embed/, /live/, or youtu.be)
-- and extract a validated 'YouTubeVideoId'.
parseYouTubeUrl :: Text -> Either Text YouTubeVideoId
parseYouTubeUrl raw = do
  uri <- maybe invalid Right (parseURI (T.unpack (T.strip raw)))
  authority <- maybe invalid Right (uriAuthority uri)
  let scheme = T.pack (uriScheme uri)
      host = T.toLower (T.pack (uriRegName authority))
      pathParts = filter (not . T.null) (T.splitOn "/" (T.pack (uriPath uri)))
      candidate
        | scheme `elem` youTubeUrlSpec.specSchemes && host `elem` youTubeUrlSpec.specPrimaryHosts =
            case pathParts of
              [p] | p == youTubeUrlSpec.specWatchPath -> queryValue youTubeUrlSpec.specWatchParam (T.pack (uriQuery uri))
              [kind, video] | kind `elem` youTubeUrlSpec.specPathPrefixes -> Just video
              _ -> Nothing
        | scheme `elem` youTubeUrlSpec.specSchemes && host `elem` youTubeUrlSpec.specShortHosts =
            case pathParts of
              [video] -> Just video
              _ -> Nothing
        | otherwise = Nothing
  videoText <- maybe invalid Right candidate
  either (const invalid) Right (mkYouTubeVideoId videoText)
  where
    invalid = Left "Enter a YouTube video link (youtube.com/watch?v=… or youtu.be/…)."
    queryValue key =
      lookup key
        . map (\field -> let (name, value) = T.breakOn "=" field in (name, T.drop 1 value))
        . T.splitOn "&"
        . T.drop 1

instance FromJSON YouTubeVideoId where
  parseJSON = withText "YouTubeVideoId" $ \t ->
    case mkYouTubeVideoId t of
      Left err  -> fail ("Invalid YouTube video ID: " <> T.unpack (renderYouTubeVideoIdError err))
      Right vid -> pure vid

instance FromHttpApiData YouTubeVideoId where
  parseUrlPiece t =
    case mkYouTubeVideoId t of
      Left err  -> Left (renderYouTubeVideoIdError err)
      Right vid -> Right vid
