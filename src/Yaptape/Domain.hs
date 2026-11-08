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
  , mixtapeIdToUUID
  , mixtapeIdFromUUID
  , ShareCode
  , shareCodeFor
  , mixtapeIdFromShareCode
  , TrackId
  , unTrackId
  , mkTrackId
  , trackIdToUUID
  , trackIdFromUUID

    -- * Domain Entities & Validation
  , Track (videoId, title, artist, note)
  , mkTrack
  , StoredTrack (..)
  , Mixtape (title, description, tracks)
  , mkMixtape
  , StoredMixtape (..)

    -- * Domain Validation Limits
  , maxMixtapeTitleLength
  , maxMixtapeDescriptionLength
  , maxMixtapeTracks
  , maxTrackTitleLength
  , maxTrackArtistLength
  , maxTrackNoteLength
  ) where

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

mixtapeIdToUUID :: MixtapeId -> UUID.UUID
mixtapeIdToUUID (MixtapeId uuid) = uuid

mixtapeIdFromUUID :: UUID.UUID -> MixtapeId
mixtapeIdFromUUID = MixtapeId

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

trackIdToUUID :: TrackId -> UUID.UUID
trackIdToUUID (TrackId uuid) = uuid

trackIdFromUUID :: UUID.UUID -> TrackId
trackIdFromUUID = TrackId

instance ToJSON TrackId where
  toJSON = String . unTrackId

instance FromJSON TrackId where
  parseJSON = withText "TrackId" $ maybe (fail "expected a UUID") pure . mkTrackId

instance ToHttpApiData TrackId where
  toUrlPiece = unTrackId

instance FromHttpApiData TrackId where
  parseUrlPiece = maybe (Left "expected a UUID") Right . mkTrackId

-- | Maximum allowed character length for a mixtape title.
maxMixtapeTitleLength :: Int
maxMixtapeTitleLength = 120

-- | Maximum allowed character length for a mixtape description.
maxMixtapeDescriptionLength :: Int
maxMixtapeDescriptionLength = 500

-- | Maximum number of tracks allowed in a single mixtape.
maxMixtapeTracks :: Int
maxMixtapeTracks = 100

-- | Maximum allowed character length for a track title.
maxTrackTitleLength :: Int
maxTrackTitleLength = 300

-- | Maximum allowed character length for a track artist name.
maxTrackArtistLength :: Int
maxTrackArtistLength = 200

-- | Maximum allowed character length for a track note.
maxTrackNoteLength :: Int
maxTrackNoteLength = 2000

-- | A track within a mixtape as received from the client for creation.
data Track = Track
  { videoId :: YouTubeVideoId
  , title   :: Maybe Text
  , artist  :: Maybe Text
  , note    :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)

-- | Smart constructor validating domain constraints on a 'Track'.
mkTrack :: YouTubeVideoId -> Maybe Text -> Maybe Text -> Text -> Either Text Track
mkTrack video trackTitle performer trackNote
  | T.null strippedNote = Left "Add a note for every track."
  | T.length strippedNote > maxTrackNoteLength = Left "Keep each note under 2000 characters."
  | exceedsLimit maxTrackTitleLength trackTitle = Left "Keep each track title under 300 characters."
  | exceedsLimit maxTrackArtistLength performer = Left "Keep each artist name under 200 characters."
  | otherwise = Right (Track video (stripNonEmpty trackTitle) (stripNonEmpty performer) strippedNote)
  where
    strippedNote = T.strip trackNote

instance FromJSON Track where
  parseJSON = withObject "Track" $ \o -> do
    video <- o .: "videoId"
    trackTitle <- o .:? "title"
    performer <- o .:? "artist"
    trackNote <- o .: "note"
    either (fail . T.unpack) pure (mkTrack video trackTitle performer trackNote)

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

-- | Smart constructor validating domain constraints on a 'Mixtape'.
mkMixtape :: Text -> Maybe Text -> [Track] -> Either Text Mixtape
mkMixtape tapeTitle tapeDescription tapeTracks
  | T.null strippedTitle = Left "Give your mixtape a title."
  | T.length strippedTitle > maxMixtapeTitleLength = Left "Keep your mixtape title under 120 characters."
  | null tapeTracks = Left "Add at least one track."
  | length tapeTracks > maxMixtapeTracks = Left "A mixtape can have at most 100 tracks."
  | exceedsLimit maxMixtapeDescriptionLength tapeDescription = Left "Keep the description under 500 characters."
  | otherwise = Right (Mixtape strippedTitle (stripNonEmpty tapeDescription) tapeTracks)
  where
    strippedTitle = T.strip tapeTitle

instance FromJSON Mixtape where
  parseJSON = withObject "Mixtape" $ \o -> do
    tapeTitle <- o .: "title"
    tapeDescription <- o .:? "description"
    tapeTracks <- o .: "tracks"
    either (fail . T.unpack) pure (mkMixtape tapeTitle tapeDescription tapeTracks)

exceedsLimit :: Int -> Maybe Text -> Bool
exceedsLimit _ Nothing = False
exceedsLimit limit (Just value) = T.length (T.strip value) > limit

stripNonEmpty :: Maybe Text -> Maybe Text
stripNonEmpty = (>>= \value -> let stripped = T.strip value in if T.null stripped then Nothing else Just stripped)

-- | A mixtape enriched with server-side metadata (ID, creation timestamp, and stored tracks).
data StoredMixtape = StoredMixtape
  { mixtapeId   :: MixtapeId
  , title       :: Text
  , description :: Maybe Text
  , createdAt   :: UTCTime
  , tracks      :: [StoredTrack]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
