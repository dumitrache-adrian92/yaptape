{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Db.Postgres
  ( createPool
  , createMixtape
  , getMixtape
  ) where

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int32)
import Data.Maybe (fromMaybe, mapMaybe)
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
import Yaptape.Db.Schema (MixtapeRow (..), TrackRow (..), fromDbRows)
import Yaptape.Domain
  ( Mixtape (..)
  , MixtapeId
  , unMixtapeId
  , mkMixtapeId
  , StoredMixtape (..)
  , Track (..)
  , mkTrackId
  )
import Yaptape.YouTube (mkYouTubeVideoId, unYouTubeVideoId, renderYouTubeVideoIdError)

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
    (mId, createdTime) <- Transaction.statement
      (mixtape.title, mixtape.description)
      insertMixtape
    let trackOrders = [0 :: Int32 .. fromIntegral (length mixtape.tracks - 1)]
        videoIds = map (unYouTubeVideoId . (\t -> t.videoId)) mixtape.tracks
        titles = map (\t -> t.title) mixtape.tracks
        artists = map (\t -> t.artist) mixtape.tracks
        notes = map (\t -> t.note) mixtape.tracks
    trackRows <- Transaction.statement
      (unMixtapeId mId, trackOrders, videoIds, titles, artists, notes)
      insertTracksBatch
    let mRow = MixtapeRow
          { mixtapeId = mId
          , title = mixtape.title
          , description = mixtape.description
          , createdAt = createdTime
          }
    pure (fromDbRows mRow trackRows))

getMixtape :: Pool.Pool -> MixtapeId -> IO (Either Pool.UsageError (Maybe StoredMixtape))
getMixtape pool mixtapeKey = Pool.use pool $ Transactions.transaction
  Transactions.ReadCommitted
  Transactions.Read
  (do
    rows <- Transaction.statement (unMixtapeId mixtapeKey) selectMixtapeWithTracks
    pure $ case rows of
      [] -> Nothing
      (mRow, firstTrack) : rest ->
        let trackRows = maybe id (:) firstTrack (mapMaybe snd rest)
        in Just (fromDbRows mRow trackRows))

selectMixtapeWithTracks :: Statement.Statement Text [(MixtapeRow, Maybe TrackRow)]
selectMixtapeWithTracks = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT m.id::text, m.title, m.description, m.created_at, t.id::text, t.mixtape_id::text, t.track_order, t.video_id, t.title, t.artist, t.note FROM mixtapes m LEFT JOIN tracks t ON t.mixtape_id = m.id WHERE m.id = $1::uuid ORDER BY t.track_order"
    encoder = Encoders.param (Encoders.nonNullable Encoders.text)
    decoder = Decoders.rowList $ do
      mId <- (Decoders.column . Decoders.nonNullable) (Decoders.refine (maybe (Left "invalid mixtape UUID") Right . mkMixtapeId) Decoders.text)
      mTitle <- Decoders.column (Decoders.nonNullable Decoders.text)
      mDesc <- Decoders.column (Decoders.nullable Decoders.text)
      mCreated <- Decoders.column (Decoders.nonNullable Decoders.timestamptz)
      let mRow = MixtapeRow mId mTitle mDesc mCreated

      mTrackId <- (Decoders.column . Decoders.nullable) (Decoders.refine (maybe (Left "invalid track UUID") Right . mkTrackId) Decoders.text)
      mTrackTapeId <- (Decoders.column . Decoders.nullable) (Decoders.refine (maybe (Left "invalid mixtape UUID") Right . mkMixtapeId) Decoders.text)
      mTrackOrder <- fmap (fmap fromIntegral) (Decoders.column (Decoders.nullable Decoders.int4))
      mVideoId <- (Decoders.column . Decoders.nullable) (Decoders.refine (either (Left . renderYouTubeVideoIdError) Right . mkYouTubeVideoId) Decoders.text)
      tTitle <- Decoders.column (Decoders.nullable Decoders.text)
      tArtist <- Decoders.column (Decoders.nullable Decoders.text)
      tNote <- Decoders.column (Decoders.nullable Decoders.text)

      let mTrackRow = case (mTrackId, mTrackTapeId, mTrackOrder, mVideoId, tNote) of
            (Just trId, Just trTapeId, Just trOrder, Just trVid, Just trNote) ->
              Just (TrackRow trId trTapeId trOrder trVid tTitle tArtist trNote)
            _ -> Nothing

      pure (mRow, mTrackRow)

insertMixtape :: Statement.Statement (Text, Maybe Text) (MixtapeId, UTCTime)
insertMixtape = Statement.Statement sql encoder decoder True
  where
    sql = "INSERT INTO mixtapes (title, description) VALUES ($1, $2) RETURNING id::text, created_at"
    encoder =
      (fst >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      (snd >$< Encoders.param (Encoders.nullable Encoders.text))
    decoder = Decoders.singleRow $ (,)
      <$> (Decoders.column . Decoders.nonNullable) (Decoders.refine (maybe (Left "invalid mixtape UUID") Right . mkMixtapeId) Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)

insertTracksBatch :: Statement.Statement (Text, [Int32], [Text], [Maybe Text], [Maybe Text], [Text]) [TrackRow]
insertTracksBatch = Statement.Statement sql encoder decoder True
  where
    sql =
      "INSERT INTO tracks (mixtape_id, track_order, video_id, title, artist, note) " <>
      "SELECT $1::uuid, t.track_order, t.video_id, t.title, t.artist, t.note " <>
      "FROM UNNEST($2::int4[], $3::text[], $4::text[], $5::text[], $6::text[]) " <>
      "  AS t(track_order, video_id, title, artist, note) " <>
      "ORDER BY t.track_order " <>
      "RETURNING id::text, mixtape_id::text, track_order, video_id, title, artist, note"
    encoder =
      ((\(tapeKey, _, _, _, _, _) -> tapeKey) >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      ((\(_, trackOrders, _, _, _, _) -> trackOrders) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.int4)))) <>
      ((\(_, _, videoIds, _, _, _) -> videoIds) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text)))) <>
      ((\(_, _, _, trackTitles, _, _) -> trackTitles) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nullable Encoders.text)))) <>
      ((\(_, _, _, _, performers, _) -> performers) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nullable Encoders.text)))) <>
      ((\(_, _, _, _, _, trackNotes) -> trackNotes) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text))))
    decoder = Decoders.rowList trackRowDecoder

trackRowDecoder :: Decoders.Row TrackRow
trackRowDecoder = TrackRow
  <$> (Decoders.column . Decoders.nonNullable) (Decoders.refine (maybe (Left "invalid track UUID") Right . mkTrackId) Decoders.text)
  <*> (Decoders.column . Decoders.nonNullable) (Decoders.refine (maybe (Left "invalid mixtape UUID") Right . mkMixtapeId) Decoders.text)
  <*> (fmap fromIntegral (Decoders.column (Decoders.nonNullable Decoders.int4)))
  <*> (Decoders.column . Decoders.nonNullable) (Decoders.refine (either (Left . renderYouTubeVideoIdError) Right . mkYouTubeVideoId) Decoders.text)
  <*> Decoders.column (Decoders.nullable Decoders.text)
  <*> Decoders.column (Decoders.nullable Decoders.text)
  <*> Decoders.column (Decoders.nonNullable Decoders.text)
