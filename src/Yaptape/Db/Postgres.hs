{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Db.Postgres
  ( createPool
  , createMixtape
  , getMixtape
  ) where

import Control.Monad (forM)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Word (Word16)
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (params)
import qualified Hasql.Connection.Setting.Connection.Param as Param
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Yaptape.Domain
  ( Mixtape (..)
  , MixtapeId
  , unMixtapeId
  , mkMixtapeId
  , StoredMixtape (..)
  , StoredTrack (..)
  , Track (..)
  , TrackId
  , mkTrackId
  )
import Yaptape.YouTube (mkYouTubeVideoId, unYouTubeVideoId, YouTubeVideoId)

createPool :: IO Pool.Pool
createPool = do
  host <- env "DATABASE_HOST" "localhost"
  port <- env "DATABASE_PORT" "5432"
  portNumber <- maybe (ioError (userError "DATABASE_PORT must be a valid port number")) pure (readMaybe port :: Maybe Word16)
  user <- env "DATABASE_USER" "yaptape"
  password <- env "DATABASE_PASSWORD" "yaptape-dev"
  database <- env "DATABASE_NAME" "yaptape"
  let connectionSettings =
        connection $ params
          [ Param.host (T.pack host)
          , Param.port portNumber
          , Param.user (T.pack user)
          , Param.password (T.pack password)
          , Param.dbname (T.pack database)
          ]
      config = PoolConfig.settings
        [ PoolConfig.size 10
        , PoolConfig.staticConnectionSettings [connectionSettings]
        ]
  Pool.acquire config
  where
    env name fallback = fromMaybe fallback <$> lookupEnv name

createMixtape :: Pool.Pool -> Mixtape -> IO (Either Pool.UsageError StoredMixtape)
createMixtape pool mixtape = Pool.use pool $ Transactions.transaction
  Transactions.ReadCommitted
  Transactions.Write
  (do
    (rawMixtapeId, createdTime) <- Transaction.statement
      (mixtape.title, mixtape.description)
      insertMixtape
    storedTracks <- forM (zip [0 :: Int32 ..] mixtape.tracks) $ \(trackOrder, track) -> do
      rawTrackId <- Transaction.statement
        (rawMixtapeId, trackOrder, track.videoId, track.title, track.artist, track.note)
        insertTrack
      pure StoredTrack
        { trackId = databaseTrackId rawTrackId
        , videoId = track.videoId
        , title = track.title
        , artist = track.artist
        , note = track.note
        }
    pure StoredMixtape
      { mixtapeId = databaseMixtapeId rawMixtapeId
      , title = mixtape.title
      , description = mixtape.description
      , createdAt = createdTime
      , tracks = storedTracks
      })

-- PostgreSQL generates these UUIDs; reject an impossible malformed return value.
databaseMixtapeId :: Text -> MixtapeId
databaseMixtapeId raw = maybe (error "PostgreSQL returned an invalid mixtape UUID") id (mkMixtapeId raw)

databaseTrackId :: Text -> TrackId
databaseTrackId raw = maybe (error "PostgreSQL returned an invalid track UUID") id (mkTrackId raw)

getMixtape :: Pool.Pool -> MixtapeId -> IO (Either Pool.UsageError (Maybe StoredMixtape))
getMixtape pool mixtapeKey = Pool.use pool $ Transactions.transaction
  Transactions.ReadCommitted
  Transactions.Read
  (do
    maybeMixtapeRow <- Transaction.statement (unMixtapeId mixtapeKey) selectMixtape
    case maybeMixtapeRow of
      Nothing -> pure Nothing
      Just (rawMixtapeId, tapeTitle, tapeDescription, createdTime) -> do
        rawTracks <- Transaction.statement (unMixtapeId mixtapeKey) selectTracks
        pure $ Just StoredMixtape
          { mixtapeId = databaseMixtapeId rawMixtapeId
          , title = tapeTitle
          , description = tapeDescription
          , createdAt = createdTime
          , tracks = map toStoredTrack rawTracks
          })
  where
    toStoredTrack (rawTrackId, rawVideoId, trackTitle, performer, trackNote) = StoredTrack
      { trackId = databaseTrackId rawTrackId
      , videoId = databaseVideoId rawVideoId
      , title = trackTitle
      , artist = performer
      , note = trackNote
      }

databaseVideoId :: Text -> YouTubeVideoId
databaseVideoId raw = either (error . show) id (mkYouTubeVideoId raw)

selectMixtape :: Statement.Statement Text (Maybe (Text, Text, Maybe Text, UTCTime))
selectMixtape = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id::text, title, description, created_at FROM mixtapes WHERE id = $1::uuid"
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.rowMaybe $ (,,,)
      <$> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)

selectTracks :: Statement.Statement Text [(Text, Text, Maybe Text, Maybe Text, Text)]
selectTracks = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT id::text, video_id, title, artist, note FROM tracks WHERE mixtape_id = $1::uuid ORDER BY track_order"
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.rowList $ (,,,,)
      <$> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nullable Decoders.text)
      <*> Decoders.column (Decoders.nullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)

insertMixtape :: Statement.Statement (Text, Maybe Text) (Text, UTCTime)
insertMixtape = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO mixtapes (title, description) VALUES ($1, $2) RETURNING id::text, created_at"
    encoder =
      (fst >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      (snd >$< Encoders.param (Encoders.nullable Encoders.text))
    decoder = Decoders.singleRow $ (,)
      <$> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)

insertTrack :: Statement.Statement (Text, Int32, YouTubeVideoId, Maybe Text, Maybe Text, Text) Text
insertTrack = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO tracks (mixtape_id, track_order, video_id, title, artist, note) VALUES ($1::uuid, $2, $3, $4, $5, $6) RETURNING id::text"
    encoder =
      ((\(tapeKey, _, _, _, _, _) -> tapeKey) >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      ((\(_, trackOrder, _, _, _, _) -> trackOrder) >$< Encoders.param (Encoders.nonNullable Encoders.int4)) <>
      ((\(_, _, youtubeId, _, _, _) -> unYouTubeVideoId youtubeId) >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      ((\(_, _, _, trackTitle, _, _) -> trackTitle) >$< Encoders.param (Encoders.nullable Encoders.text)) <>
      ((\(_, _, _, _, performer, _) -> performer) >$< Encoders.param (Encoders.nullable Encoders.text)) <>
      ((\(_, _, _, _, _, trackNote) -> trackNote) >$< Encoders.param (Encoders.nonNullable Encoders.text))
    decoder = Decoders.singleRow $ Decoders.column (Decoders.nonNullable Decoders.text)
