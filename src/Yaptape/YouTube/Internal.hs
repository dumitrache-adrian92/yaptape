{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Yaptape.YouTube.Internal
  ( YouTubeVideoId (..)
  , YouTubeVideoIdError (..)
  , mkYouTubeVideoId
  , unsafeMkYouTubeVideoId
  , renderYouTubeVideoIdError
  ) where

import Data.Aeson (FromJSON (..), ToJSON, withText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
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

-- | Validate candidate characters and length.
-- YouTube video IDs are strictly 11 base64url ASCII characters: [a-zA-Z0-9_-].
validateCandidate :: Text -> Either YouTubeVideoIdError YouTubeVideoId
validateCandidate cand
  | T.null cand = Left EmptyInput
  | T.length cand /= 11 = Left (InvalidLength (T.length cand))
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
-- Normalizes input from URLs or raw strings and validates length & characters.
mkYouTubeVideoId :: Text -> Either YouTubeVideoIdError YouTubeVideoId
mkYouTubeVideoId = validateCandidate . T.strip

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
