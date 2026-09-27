{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Yaptape.Domain
  ( -- * Identifiers
    MixtapeId (..)
  , TrackId (..)

    -- * Domain Entities
  , Track (..)
  , StoredTrack (..)
  , Mixtape (..)
  , StoredMixtape (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData)

import Yaptape.YouTube (YouTubeVideoId)

-- | Unique identifier for a mixtape.
newtype MixtapeId = MixtapeId { unMixtapeId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

-- | Unique identifier for a track row.
newtype TrackId = TrackId { unTrackId :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

-- | A track within a mixtape as received from the client for creation.
data Track = Track
  { videoId :: YouTubeVideoId
  , title   :: Maybe Text
  , artist  :: Maybe Text
  , note    :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
    deriving anyclass (ToJSON, FromJSON)

-- | A mixtape enriched with server-side metadata (ID, creation timestamp, and stored tracks).
data StoredMixtape = StoredMixtape
  { mixtapeId   :: MixtapeId
  , title       :: Text
  , description :: Maybe Text
  , createdAt   :: UTCTime
  , tracks      :: [StoredTrack]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
