{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Db.Schema
  ( -- * Database Row Types
    MixtapeRow (..)
  , TrackRow (..)
  , TrackBatchParams

    -- * Domain <-> Database Mappings
  , fromDbRows
  , toTrackRows
  , toTrackBatchParams
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)

import Yaptape.Domain
  ( MixtapeId
  , StoredMixtape (..)
  , StoredTrack (..)
  , Track (videoId, title, artist, note)
  , TrackId
  , mixtapeIdToUUID
  )
import Yaptape.YouTube (YouTubeVideoId, unYouTubeVideoId)

-- | Database row representing a record in the 'mixtapes' table.
data MixtapeRow = MixtapeRow
  { mixtapeId   :: MixtapeId
  , title       :: Text
  , description :: Maybe Text
  , createdAt   :: UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Database row representing a record in the 'tracks' table.
-- Maintains order in the mixtape via 'trackOrder' and references 'mixtapeId'.
data TrackRow = TrackRow
  { trackId    :: TrackId
  , mixtapeId  :: MixtapeId
  , trackOrder :: Int
  , videoId    :: YouTubeVideoId
  , title      :: Maybe Text
  , artist     :: Maybe Text
  , note       :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Reconstruct a domain 'StoredMixtape' aggregate from relational database rows.
fromDbRows :: MixtapeRow -> [TrackRow] -> StoredMixtape
fromDbRows mRow tRows =
  let sortedTracks = List.sortOn (\t -> t.trackOrder) tRows
      storedTracks = map toStoredTrack sortedTracks
  in StoredMixtape
    { mixtapeId   = mRow.mixtapeId
    , title       = mRow.title
    , description = mRow.description
    , createdAt   = mRow.createdAt
    , tracks      = storedTracks
    }
  where
    toStoredTrack :: TrackRow -> StoredTrack
    toStoredTrack t = StoredTrack
      { trackId = t.trackId
      , videoId = t.videoId
      , title   = t.title
      , artist  = t.artist
      , note    = t.note
      }

-- | Convert a list of domain 'Track's to relational 'TrackRow's,
-- assigning 0-indexed 'trackOrder' and applying a custom ID generator.
toTrackRows :: MixtapeId -> (Int -> TrackId) -> [Track] -> [TrackRow]
toTrackRows mId makeTrackId trks =
  zipWith (\idx t -> TrackRow
    { trackId = makeTrackId idx
    , mixtapeId = mId
    , trackOrder = idx
    , videoId = t.videoId
    , title = t.title
    , artist = t.artist
    , note = t.note
    }) [0..] trks

-- | Columnar batch parameters for PostgreSQL @UNNEST@ track insertion.
type TrackBatchParams = (UUID.UUID, [Int32], [Text], [Maybe Text], [Maybe Text], [Text])

-- | Convert a 'MixtapeId' and domain 'Track' list into columnar arrays for batch insertion.
toTrackBatchParams :: MixtapeId -> [Track] -> TrackBatchParams
toTrackBatchParams mId trks =
  ( mixtapeIdToUUID mId
  , [0 :: Int32 .. fromIntegral (length trks - 1)]
  , map (unYouTubeVideoId . (\t -> t.videoId)) trks
  , map (\t -> t.title) trks
  , map (\t -> t.artist) trks
  , map (\t -> t.note) trks
  )
