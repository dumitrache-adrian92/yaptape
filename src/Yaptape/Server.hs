{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Server
  ( server
  , MixtapeStore (..)
  , StoreError (..)
  , postgresMixtapeStore
  , appForStore
  , healthHandler
  , createMixtapeHandler
  , getSharedMixtapeHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lucid (Html, a_, body_, h1_, head_, href_, html_, li_, main_, p_, title_, toHtml, ul_)
import Network.Wai (Application)
import Network.HTTP.Types.Header (hContentType)
import Servant
  ( (:<|>) (..)
  , Handler
  , Header
  , Headers
  , Server
  , ServerError (..)
  , addHeader
  , err404
  , err500
  , err503
  , throwError
  )
import Servant (serve)
import qualified Hasql.Pool as Pool
import System.IO (hPutStrLn, stderr)
import Yaptape.Api (AppApi)
import Yaptape.Db.Postgres (createMixtape, getMixtape)
import Yaptape.Domain
  ( Mixtape
  , MixtapeId
  , ShareCode
  , StoredMixtape (..)
  , StoredTrack (..)
  , mixtapeIdFromShareCode
  , shareCodeFor
  )
import Yaptape.YouTube (unYouTubeVideoId)
import Servant.API (ToHttpApiData (toUrlPiece))
import Yaptape.Api (appApi)

data StoreError = StoreUnavailable | StoreFailure
  deriving (Show, Eq)

data MixtapeStore = MixtapeStore
  { storeCreateMixtape :: Mixtape -> IO (Either StoreError StoredMixtape)
  , storeGetMixtape :: MixtapeId -> IO (Either StoreError (Maybe StoredMixtape))
  }

postgresMixtapeStore :: Pool.Pool -> MixtapeStore
postgresMixtapeStore pool = MixtapeStore
  { storeCreateMixtape = \mixtape -> mapPoolError <$> createMixtape pool mixtape
  , storeGetMixtape = \tapeId -> mapPoolError <$> getMixtape pool tapeId
  }

mapPoolError :: Either Pool.UsageError a -> Either StoreError a
mapPoolError result = case result of
  Left (Pool.ConnectionUsageError _) -> Left StoreUnavailable
  Left Pool.AcquisitionTimeoutUsageError -> Left StoreUnavailable
  Left (Pool.SessionUsageError _) -> Left StoreFailure
  Right value -> Right value

appForStore :: MixtapeStore -> Application
appForStore = serve appApi . server

server :: MixtapeStore -> Server AppApi
server store = healthHandler :<|> (createMixtapeHandler store :<|> getMixtapeHandler store) :<|>
  (pageHandler :<|> getSharedMixtapeHandler store)

healthHandler :: Handler String
healthHandler = return "https://www.youtube.com/watch?v=_rVvjslF6M8"

pageHandler :: Handler (Html ())
pageHandler = pure $ html_ $ do
  head_ (title_ "Yaptape")
  body_ (main_ (h1_ "Yaptape"))

createMixtapeHandler
  :: MixtapeStore
  -> Mixtape
  -> Handler (Headers '[Header "Location" Text] StoredMixtape)
createMixtapeHandler store mixtape = do
  result <- liftIO (storeCreateMixtape store mixtape)
  case result of
    Left storeError -> do
      liftIO $ hPutStrLn stderr ("Mixtape persistence failed: " ++ show storeError)
      throwError (storeErrorResponse storeError)
    Right created -> pure $ addHeader
      ("/m/" <> toUrlPiece (shareCodeFor created.mixtapeId))
      created

getMixtapeHandler :: MixtapeStore -> MixtapeId -> Handler StoredMixtape
getMixtapeHandler store tapeId = do
  result <- liftIO (storeGetMixtape store tapeId)
  case result of
    Left storeError -> do
      liftIO $ hPutStrLn stderr ("Mixtape retrieval failed: " ++ show storeError)
      throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure mixtape

getSharedMixtapeHandler :: MixtapeStore -> ShareCode -> Handler (Html ())
getSharedMixtapeHandler store shareCode = do
  result <- liftIO (storeGetMixtape store (mixtapeIdFromShareCode shareCode))
  case result of
    Left storeError -> do
      liftIO $ hPutStrLn stderr ("Shared mixtape retrieval failed: " ++ show storeError)
      throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure (renderMixtapePage mixtape)

renderMixtapePage :: StoredMixtape -> Html ()
renderMixtapePage mixtape = html_ $ do
  head_ (title_ (toHtml mixtape.title))
  body_ $ main_ $ do
    h1_ (toHtml mixtape.title)
    maybe (pure ()) (p_ . toHtml) mixtape.description
    ul_ (mapM_ renderTrack mixtape.tracks)

renderTrack :: StoredTrack -> Html ()
renderTrack track = li_ $ do
  let youtubeUrl = "https://www.youtube.com/watch?v=" <> unYouTubeVideoId track.videoId
      label = fromMaybe (unYouTubeVideoId track.videoId) track.title
  a_ [href_ youtubeUrl] (toHtml label)
  maybe (pure ()) (p_ . toHtml) track.artist
  p_ (toHtml track.note)

storeErrorResponse :: StoreError -> ServerError
storeErrorResponse storeError =
  case storeError of
    StoreUnavailable -> jsonError err503 "storage_unavailable"
    StoreFailure -> jsonError err500 "internal_error"

jsonError :: ServerError -> String -> ServerError
jsonError response errorCode = response
  { errBody = encode (object ["error" .= errorCode])
  , errHeaders = (hContentType, "application/json") : errHeaders response
  }
