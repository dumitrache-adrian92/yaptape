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
import qualified Data.ByteString.Char8 as BS8
import Data.Char (toLower)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import GHC.Generics (Generic)
import Network.HTTP.Types.URI (parseSimpleQuery)
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

-- | Extract the candidate 11-character video ID from a raw text or URL.
-- Supported URL formats:
--   - https://www.youtube.com/watch?v=...
--   - https://youtu.be/...
--   - https://www.youtube.com/shorts/...
--   - https://www.youtube.com/embed/...
--   - Raw 11-character ID
extractCandidate :: Text -> Text
extractCandidate rawInput
  | T.null trimmed = trimmed
  | isCandidateLike trimmed = trimmed
  | otherwise = case parseAsUri trimmed of
      Just uri -> extractFromUri uri
      Nothing  -> trimmed
  where
    trimmed = T.strip rawInput

    isCandidateLike t =
      T.length t == 11 && T.all (\c -> c /= '/' && c /= ':' && c /= '?' && c /= '&' && c /= '#') t

    parseAsUri t =
      let s = T.unpack t
      in case parseURI s of
        Just uri -> Just uri
        Nothing
          | "youtu" `T.isInfixOf` t -> parseURI ("https://" <> s)
          | otherwise              -> Nothing

    extractFromUri uri =
      let host = maybe "" (map toLower . uriRegName) (uriAuthority uri)
          path = uriPath uri
          query = parseSimpleQuery (BS8.pack (uriQuery uri))
      in if host == "youtu.be" || ".youtu.be" `isSuffixOf` host
           then extractShortUrlPath path
           else if host == "youtube.com" || ".youtube.com" `isSuffixOf` host
             then case lookup "v" query of
               Just vBytes | not (BS8.null vBytes) -> TE.decodeUtf8With TEE.lenientDecode vBytes
               _ -> if "/shorts/" `isPrefixOf` path || "/shorts/" `isInfixOf` path
                      then extractSegmentAfter "/shorts/" (T.pack path)
                      else if "/embed/" `isPrefixOf` path || "/embed/" `isInfixOf` path
                        then extractSegmentAfter "/embed/" (T.pack path)
                        else trimmed
             else trimmed

    extractShortUrlPath p =
      let clean = dropWhile (== '/') p
      in T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '&' && c /= '#') (T.pack clean)

    extractSegmentAfter marker text =
      let after = snd (T.breakOnEnd marker text)
      in T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '&' && c /= '#') after

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
mkYouTubeVideoId = validateCandidate . extractCandidate

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
