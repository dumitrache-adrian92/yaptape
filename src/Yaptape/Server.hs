{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Server
  ( server
  , MixtapeStore (..)
  , StoreError (..)
  , postgresMixtapeStore
  , appForStore
  , appForStoreAt
  , healthHandler
  , createMixtapeHandler
  , createdPageHandler
  , getSharedMixtapeHandler
  ) where

import Control.Exception (AsyncException, SomeException, fromException, tryJust)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.ByteString as BS
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Lucid (Html, renderBS)
import Network.Wai
  ( Application
  , Request (..)
  , Response
  , mapResponseHeaders
  , responseLBS
  , getRequestBodyChunk
  , setRequestBodyChunks
  , pathInfo
  )
import Network.HTTP.Types.Header (hContentType, hLocation, hCacheControl)
import Network.HTTP.Types (status413)
import Servant
  ( (:<|>) (..)
  , Handler
  , Header
  , Headers
  , Server
  , ServerError (..)
  , addHeader
  , err303
  , err404
  , err422
  , err500
  , err503
  , throwError
  , serveDirectoryWebApp
  )
import Servant (serve)
import Servant.API (ToHttpApiData (toUrlPiece))
import qualified Hasql.Pool as Pool
import System.IO (hPutStrLn, stderr)
import Yaptape.Api (AppApi)
import Yaptape.Db.Postgres (createMixtape, getMixtape)
import Yaptape.Domain
  ( Mixtape
  , MixtapeId
  , ShareCode
  , StoredMixtape (..)
  , mixtapeIdFromShareCode
  , shareCodeFor
  )
import Yaptape.Api (appApi)
import Yaptape.Form (CreateMixtapeForm, createMixtapeFromForm)
import Yaptape.Pages (renderCreatedPage, renderCreatePage, renderLandingPage, renderMixtapePage)

data StoreError = StoreUnavailable | StoreFailure
  deriving (Show, Eq)

data MixtapeStore = MixtapeStore
  { storeCreateMixtape :: Mixtape -> IO (Either StoreError StoredMixtape)
  , storeGetMixtape :: MixtapeId -> IO (Either StoreError (Maybe StoredMixtape))
  }

postgresMixtapeStore :: Pool.Pool -> MixtapeStore
postgresMixtapeStore pool = MixtapeStore
  { storeCreateMixtape = \mixtape -> runDb "create mixtape" (createMixtape pool mixtape)
  , storeGetMixtape = \tapeId -> runDb "get mixtape" (getMixtape pool tapeId)
  }

runDb :: String -> IO (Either Pool.UsageError a) -> IO (Either StoreError a)
runDb operation action = do
  outcome <- tryJust synchronousException action
  case outcome of
    Left exception -> do
      hPutStrLn stderr ("Database " ++ operation ++ " failed: " ++ show exception)
      pure (Left StoreFailure)
    Right result -> do
      case result of
        Left dbError -> hPutStrLn stderr ("Database " ++ operation ++ " failed: " ++ show dbError)
        Right _ -> pure ()
      pure (mapPoolError result)
  where
    synchronousException exception = case fromException exception :: Maybe AsyncException of
      Just _ -> Nothing
      Nothing -> Just (exception :: SomeException)

mapPoolError :: Either Pool.UsageError a -> Either StoreError a
mapPoolError result = case result of
  Left (Pool.ConnectionUsageError _) -> Left StoreUnavailable
  Left Pool.AcquisitionTimeoutUsageError -> Left StoreUnavailable
  Left (Pool.SessionUsageError _) -> Left StoreFailure
  Right value -> Right value

appForStore :: MixtapeStore -> Application
appForStore = (`appForStoreAt` "static")

appForStoreAt :: MixtapeStore -> FilePath -> Application
appForStoreAt store directory = bodySizeLimit (1024 * 1024) withSecurityHeaders (cacheControlMiddleware app)
  where
    app = serve appApi (server store directory)
    csp = "default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'self'; script-src 'self' https://www.youtube.com https://s.ytimg.com; style-src 'self' https://fonts.googleapis.com; font-src 'self' https://fonts.gstatic.com; img-src 'self' data: https://i.ytimg.com; frame-src https://www.youtube.com https://www.youtube-nocookie.com; connect-src 'self' https://www.youtube.com"
    securityHeaders =
      [ ("Content-Security-Policy", csp)
      , ("Referrer-Policy", "strict-origin-when-cross-origin")
      , ("X-Content-Type-Options", "nosniff")
      , ("X-Frame-Options", "SAMEORIGIN")
      ]
    withSecurityHeaders = mapResponseHeaders (securityHeaders ++)
    cacheControlMiddleware innerApp req sendResp =
      innerApp req $ \resp ->
        case pathInfo req of
          ("assets" : _) -> sendResp (mapResponseHeaders ((hCacheControl, "public, max-age=86400") :) resp)
          _ -> sendResp resp

bodySizeLimit :: Int -> (Response -> Response) -> Application -> Application
bodySizeLimit limit secureResponse app request respond
  | requestMethod request `notElem` ["POST", "PUT", "PATCH"] = app request respond
  | otherwise = do
      body <- collect 0 []
      case body of
        Nothing -> respond $ secureResponse $ responseLBS
          status413
          [(hContentType, "text/plain; charset=utf-8")]
          "Request body too large"
        Just chunks -> do
          bodyRef <- newIORef chunks
          let getChunk = atomicModifyIORef' bodyRef $ \remaining ->
                case remaining of
                  [] -> ([], BS.empty)
                  chunk : rest -> (rest, chunk)
          app (setRequestBodyChunks getChunk request) (respond . secureResponse)
  where
    collect size chunks = do
      chunk <- getRequestBodyChunk request
      if BS.null chunk
        then pure (Just (reverse chunks))
        else if size + BS.length chunk > limit
          then pure Nothing
          else collect (size + BS.length chunk) (chunk : chunks)

server :: MixtapeStore -> FilePath -> Server AppApi
server store staticDirectory = healthHandler :<|> (createMixtapeHandler store :<|> getMixtapeHandler store) :<|>
  (pageHandler :<|> ((createPageHandler :<|> submitCreateFormHandler store) :<|> (createdPageHandler store :<|> getSharedMixtapeHandler store))) :<|>
  serveDirectoryWebApp staticDirectory

healthHandler :: Handler String
healthHandler = return "https://www.youtube.com/watch?v=_rVvjslF6M8"

pageHandler :: Handler (Html ())
pageHandler = pure renderLandingPage

createPageHandler :: Handler (Html ())
createPageHandler = pure (renderCreatePage Nothing Nothing)

submitCreateFormHandler :: MixtapeStore -> CreateMixtapeForm -> Handler (Html ())
submitCreateFormHandler store form = case createMixtapeFromForm form of
  Left validationError -> throwError err422
    { errBody = renderBS (renderCreatePage (Just validationError) (Just form))
    , errHeaders = [(hContentType, "text/html; charset=utf-8")]
    }
  Right mixtape -> do
    result <- liftIO (storeCreateMixtape store mixtape)
    case result of
      Left _ -> throwError err500
        { errBody = renderBS (renderCreatePage (Just "We could not save your mixtape. Please try again.") (Just form))
        , errHeaders = [(hContentType, "text/html; charset=utf-8")]
        }
      Right created -> do
        let target = "/created/" <> toUrlPiece (shareCodeFor created.mixtapeId)
            targetBs = TE.encodeUtf8 target
        throwError err303
          { errHeaders =
              [ (hLocation, targetBs)
              , ("HX-Redirect", targetBs)
              ]
          }

createdPageHandler :: MixtapeStore -> ShareCode -> Handler (Html ())
createdPageHandler store shareCode = do
  result <- liftIO (storeGetMixtape store (mixtapeIdFromShareCode shareCode))
  case result of
    Left storeError -> throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure (renderCreatedPage mixtape)

createMixtapeHandler
  :: MixtapeStore
  -> Mixtape
  -> Handler (Headers '[Header "Location" Text] StoredMixtape)
createMixtapeHandler store mixtape = do
  result <- liftIO (storeCreateMixtape store mixtape)
  case result of
    Left storeError -> do
      throwError (storeErrorResponse storeError)
    Right created -> pure $ addHeader
      ("/m/" <> toUrlPiece (shareCodeFor created.mixtapeId))
      created

getMixtapeHandler :: MixtapeStore -> MixtapeId -> Handler StoredMixtape
getMixtapeHandler store tapeId = do
  result <- liftIO (storeGetMixtape store tapeId)
  case result of
    Left storeError -> do
      throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure mixtape

getSharedMixtapeHandler :: MixtapeStore -> ShareCode -> Handler (Html ())
getSharedMixtapeHandler store shareCode = do
  result <- liftIO (storeGetMixtape store (mixtapeIdFromShareCode shareCode))
  case result of
    Left storeError -> throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure (renderMixtapePage mixtape)

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
