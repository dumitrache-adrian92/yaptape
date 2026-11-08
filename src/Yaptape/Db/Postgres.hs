{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Db.Postgres
  ( DbConfig (..)
  , resolveDbConfig
  , createPool
  , createPoolFromConfig
  , runMigrations
  , runMigrationsFrom
  , createMixtape
  , getMixtape
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Data.Word (Word16)
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (params)
import qualified Hasql.Connection.Setting.Connection.Param as Param
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)
import Yaptape.Db.Schema (MixtapeRow (..), TrackBatchParams, TrackRow (..), fromDbRows, toTrackBatchParams)
import Yaptape.Domain
  ( Mixtape (..)
  , MixtapeId
  , mixtapeIdFromUUID
  , mixtapeIdToUUID
  , StoredMixtape (..)
  , trackIdFromUUID
  )
import Yaptape.YouTube (mkYouTubeVideoId, renderYouTubeVideoIdError)

data DbConfig = DbConfig
  { dbHost     :: Text
  , dbPort     :: Word16
  , dbUser     :: Text
  , dbPassword :: Text
  , dbName     :: Text
  } deriving (Show, Eq)

resolveDbConfig :: IO DbConfig
resolveDbConfig = do
  host <- env "DATABASE_HOST" "localhost"
  portRaw <- env "DATABASE_PORT" "5432"
  portNumber <- maybe (ioError (userError "DATABASE_PORT must be a valid port number")) pure (readMaybe portRaw :: Maybe Word16)
  user <- env "DATABASE_USER" "yaptape"
  password <- env "DATABASE_PASSWORD" "yaptape-dev"
  database <- env "DATABASE_NAME" "yaptape"
  pure DbConfig
    { dbHost = T.pack host
    , dbPort = portNumber
    , dbUser = T.pack user
    , dbPassword = T.pack password
    , dbName = T.pack database
    }
  where
    env name fallback = fromMaybe fallback <$> lookupEnv name

createPool :: IO Pool.Pool
createPool = resolveDbConfig >>= createPoolFromConfig

createPoolFromConfig :: DbConfig -> IO Pool.Pool
createPoolFromConfig cfg = do
  let connectionSettings =
        connection $ params
          [ Param.host cfg.dbHost
          , Param.port cfg.dbPort
          , Param.user cfg.dbUser
          , Param.password cfg.dbPassword
          , Param.dbname cfg.dbName
          ]
      config = PoolConfig.settings
        [ PoolConfig.size 10
        , PoolConfig.staticConnectionSettings [connectionSettings]
        ]
  Pool.acquire config

-- | Run all @.sql@ migrations from the default @db/migrations@ directory.
runMigrations :: Pool.Pool -> IO (Either Pool.UsageError ())
runMigrations pool = runMigrationsFrom pool "db/migrations"

-- | Run all @.sql@ files in lexicographical order from the given directory.
runMigrationsFrom :: Pool.Pool -> FilePath -> IO (Either Pool.UsageError ())
runMigrationsFrom pool migrationsDir = do
  entries <- listDirectory migrationsDir
  let sqlFiles = List.sort [migrationsDir </> f | f <- entries, takeExtension f == ".sql"]
  scripts <- traverse BS.readFile sqlFiles
  Pool.use pool (forM_ scripts Session.sql)

createMixtape :: Pool.Pool -> Mixtape -> IO (Either Pool.UsageError StoredMixtape)
createMixtape pool mixtape = Pool.use pool $ Transactions.transaction
  Transactions.ReadCommitted
  Transactions.Write
  (do
    (mId, createdTime) <- Transaction.statement
      (mixtape.title, mixtape.description)
      insertMixtape
    trackRows <- Transaction.statement
      (toTrackBatchParams mId mixtape.tracks)
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
    rows <- Transaction.statement (mixtapeIdToUUID mixtapeKey) selectMixtapeWithTracks
    pure $ case rows of
      [] -> Nothing
      (mRow, firstTrack) : rest ->
        let trackRows = maybe id (:) firstTrack (mapMaybe snd rest)
        in Just (fromDbRows mRow trackRows))

selectMixtapeWithTracks :: Statement.Statement UUID.UUID [(MixtapeRow, Maybe TrackRow)]
selectMixtapeWithTracks = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT m.id, m.title, m.description, m.created_at, t.id, t.mixtape_id, t.track_order, t.video_id, t.title, t.artist, t.note FROM mixtapes m LEFT JOIN tracks t ON t.mixtape_id = m.id WHERE m.id = $1 ORDER BY t.track_order"
    encoder = Encoders.param (Encoders.nonNullable Encoders.uuid)
    decoder = Decoders.rowList $ do
      mId <- fmap mixtapeIdFromUUID (Decoders.column (Decoders.nonNullable Decoders.uuid))
      mTitle <- Decoders.column (Decoders.nonNullable Decoders.text)
      mDesc <- Decoders.column (Decoders.nullable Decoders.text)
      mCreated <- Decoders.column (Decoders.nonNullable Decoders.timestamptz)
      let mRow = MixtapeRow mId mTitle mDesc mCreated

      mTrackId <- fmap (fmap trackIdFromUUID) (Decoders.column (Decoders.nullable Decoders.uuid))
      mTrackTapeId <- fmap (fmap mixtapeIdFromUUID) (Decoders.column (Decoders.nullable Decoders.uuid))
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
    sql = "INSERT INTO mixtapes (title, description) VALUES ($1, $2) RETURNING id, created_at"
    encoder =
      (fst >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      (snd >$< Encoders.param (Encoders.nullable Encoders.text))
    decoder = Decoders.singleRow $ (,)
      <$> fmap mixtapeIdFromUUID (Decoders.column (Decoders.nonNullable Decoders.uuid))
      <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)

insertTracksBatch :: Statement.Statement TrackBatchParams [TrackRow]
insertTracksBatch = Statement.Statement sql encoder decoder True
  where
    sql =
      "INSERT INTO tracks (mixtape_id, track_order, video_id, title, artist, note) " <>
      "SELECT $1, t.track_order, t.video_id, t.title, t.artist, t.note " <>
      "FROM UNNEST($2::int4[], $3::text[], $4::text[], $5::text[], $6::text[]) " <>
      "  AS t(track_order, video_id, title, artist, note) " <>
      "ORDER BY t.track_order " <>
      "RETURNING id, mixtape_id, track_order, video_id, title, artist, note"
    encoder =
      ((\(tapeKey, _, _, _, _, _) -> tapeKey) >$< Encoders.param (Encoders.nonNullable Encoders.uuid)) <>
      ((\(_, trackOrders, _, _, _, _) -> trackOrders) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.int4)))) <>
      ((\(_, _, videoIds, _, _, _) -> videoIds) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text)))) <>
      ((\(_, _, _, trackTitles, _, _) -> trackTitles) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nullable Encoders.text)))) <>
      ((\(_, _, _, _, performers, _) -> performers) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nullable Encoders.text)))) <>
      ((\(_, _, _, _, _, trackNotes) -> trackNotes) >$< Encoders.param (Encoders.nonNullable (Encoders.foldableArray (Encoders.nonNullable Encoders.text))))
    decoder = Decoders.rowList trackRowDecoder

trackRowDecoder :: Decoders.Row TrackRow
trackRowDecoder = TrackRow
  <$> fmap trackIdFromUUID (Decoders.column (Decoders.nonNullable Decoders.uuid))
  <*> fmap mixtapeIdFromUUID (Decoders.column (Decoders.nonNullable Decoders.uuid))
  <*> (fmap fromIntegral (Decoders.column (Decoders.nonNullable Decoders.int4)))
  <*> (Decoders.column . Decoders.nonNullable) (Decoders.refine (either (Left . renderYouTubeVideoIdError) Right . mkYouTubeVideoId) Decoders.text)
  <*> Decoders.column (Decoders.nullable Decoders.text)
  <*> Decoders.column (Decoders.nullable Decoders.text)
  <*> Decoders.column (Decoders.nonNullable Decoders.text)
