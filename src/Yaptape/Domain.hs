{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Yaptape.Domain
  ( -- * Identifiers
    MixtapeId
  , unMixtapeId
  , mkMixtapeId
  , ShareCode
  , shareCodeFor
  , mixtapeIdFromShareCode
  , TrackId
  , unTrackId
  , mkTrackId

    -- * Domain Entities
  , Track (..)
  , StoredTrack (..)
  , Mixtape (..)
  , StoredMixtape (..)
  ) where

import Control.Monad (when)
import qualified Data.ByteString.Base64.URL as Base64Url
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText, withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData (..), ToHttpApiData (..))
import qualified Data.UUID as UUID

import Yaptape.YouTube (YouTubeVideoId)

-- | Unique identifier for a mixtape.
newtype MixtapeId = MixtapeId UUID.UUID
  deriving stock (Show, Eq, Ord, Generic)

unMixtapeId :: MixtapeId -> Text
unMixtapeId (MixtapeId uuid) = UUID.toText uuid

mkMixtapeId :: Text -> Maybe MixtapeId
mkMixtapeId = fmap MixtapeId . UUID.fromText

instance ToJSON MixtapeId where
  toJSON = String . unMixtapeId

instance FromJSON MixtapeId where
  parseJSON = withText "MixtapeId" $ maybe (fail "expected a UUID") pure . mkMixtapeId

instance ToHttpApiData MixtapeId where
  toUrlPiece = unMixtapeId

instance FromHttpApiData MixtapeId where
  parseUrlPiece = maybe (Left "expected a UUID") Right . mkMixtapeId

-- | A compact, URL-safe representation of a mixtape UUID.
newtype ShareCode = ShareCode MixtapeId
  deriving stock (Show, Eq, Ord)

shareCodeFor :: MixtapeId -> ShareCode
shareCodeFor = ShareCode

mixtapeIdFromShareCode :: ShareCode -> MixtapeId
mixtapeIdFromShareCode (ShareCode mixtapeKey) = mixtapeKey

instance ToHttpApiData ShareCode where
  toUrlPiece (ShareCode (MixtapeId uuid)) =
    TE.decodeUtf8 (Base64Url.encodeUnpadded (LBS.toStrict (UUID.toByteString uuid)))

instance FromHttpApiData ShareCode where
  parseUrlPiece encoded
    | T.length encoded /= 22 = Left "invalid share code"
    | otherwise = do
        bytes <- either (const (Left "invalid share code")) Right
          (Base64Url.decodeUnpadded (TE.encodeUtf8 encoded))
        uuid <- maybe (Left "invalid share code") Right (UUID.fromByteString (LBS.fromStrict bytes))
        let shareCode = ShareCode (MixtapeId uuid)
        if toUrlPiece shareCode == encoded
          then Right shareCode
          else Left "invalid share code"

-- | Unique identifier for a track row.
newtype TrackId = TrackId UUID.UUID
  deriving stock (Show, Eq, Ord, Generic)

unTrackId :: TrackId -> Text
unTrackId (TrackId uuid) = UUID.toText uuid

mkTrackId :: Text -> Maybe TrackId
mkTrackId = fmap TrackId . UUID.fromText

instance ToJSON TrackId where
  toJSON = String . unTrackId

instance FromJSON TrackId where
  parseJSON = withText "TrackId" $ maybe (fail "expected a UUID") pure . mkTrackId

instance ToHttpApiData TrackId where
  toUrlPiece = unTrackId

instance FromHttpApiData TrackId where
  parseUrlPiece = maybe (Left "expected a UUID") Right . mkTrackId

-- | A track within a mixtape as received from the client for creation.
data Track = Track
  { videoId :: YouTubeVideoId
  , title   :: Maybe Text
  , artist  :: Maybe Text
  , note    :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

instance FromJSON Track where
  parseJSON = withObject "Track" $ \o -> do
    video <- o .: "videoId"
    trackTitle <- o .:? "title"
    performer <- o .:? "artist"
    trackNote <- o .: "note"
    when (T.null (T.strip trackNote)) (fail "note must not be empty")
    pure (Track video trackTitle performer trackNote)

-- | A track that has been persisted, enriched with its unique TrackId.
data StoredTrack = StoredTrack
  { trackId :: TrackId
  , videoId :: YouTubeVideoId
  , title   :: Maybe Text
  , artist  :: Maybe Text
  , note    :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A mixtape as received from the client for creation.
data Mixtape = Mixtape
  { title       :: Text
  , description :: Maybe Text
  , tracks      :: [Track]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

instance FromJSON Mixtape where
  parseJSON = withObject "Mixtape" $ \o -> do
    tapeTitle <- o .: "title"
    tapeDescription <- o .:? "description"
    tapeTracks <- o .: "tracks"
    when (T.null (T.strip tapeTitle)) (fail "title must not be empty")
    when (null tapeTracks) (fail "a mixtape must have at least one track")
    pure (Mixtape tapeTitle tapeDescription tapeTracks)

-- | A mixtape enriched with server-side metadata (ID, creation timestamp, and stored tracks).
data StoredMixtape = StoredMixtape
  { mixtapeId   :: MixtapeId
  , title       :: Text
  , description :: Maybe Text
  , createdAt   :: UTCTime
  , tracks      :: [StoredTrack]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
