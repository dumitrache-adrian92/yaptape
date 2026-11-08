{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Server
  ( AppEnv (..)
  , AppM
  , LogLevel (..)
  , LogEntry (..)
  , Logger
  , defaultStderrLogger
  , server
  , serverT
  , MixtapeStore (..)
  , StoreError (..)
  , postgresMixtapeStore
  , postgresMixtapeStoreWithLogger
  , appForStore
  , appForStoreAt
  , appForStoreWithLogger
  , healthHandler
  , createMixtapeHandler
  , createdPageHandler
  , getSharedMixtapeHandler
  ) where

import Control.Exception (AsyncException, SomeException, fromException, tryJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.IORef (newIORef, atomicModifyIORef')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Lucid (Html, renderBS)
import Network.Wai
  ( Application
  , Request (..)
  , mapResponseHeaders
  , responseLBS
  , responseStatus
  , getRequestBodyChunk
  , setRequestBodyChunks
  , pathInfo
  , rawPathInfo
  )
import Network.HTTP.Types.Header (hContentType, hLocation, hCacheControl)
import Network.HTTP.Types (status413, statusCode)
import Servant
  ( (:<|>) (..)
  , Handler
  , Header
  , Headers
  , Server
  , ServerError (..)
  , ServerT
  , addHeader
  , err303
  , err404
  , err422
  , err500
  , err503
  , hoistServer
  , throwError
  , serveDirectoryWebApp
  )
import Servant (serve)
import Servant.API (ToHttpApiData (toUrlPiece))
import qualified Hasql.Pool as Pool
import System.IO (stderr)
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
import Yaptape.Pages (renderCreatedPage, renderCreatePage, renderErrorPage, renderLandingPage, renderMixtapePage)

data LogLevel = LogInfo | LogWarn | LogError
  deriving (Show, Eq, Ord)

renderLogLevel :: LogLevel -> Text
renderLogLevel LogInfo  = "INFO"
renderLogLevel LogWarn  = "WARN"
renderLogLevel LogError = "ERROR"

data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel     :: LogLevel
  , logEvent     :: Text
  , logMessage   :: Text
  } deriving (Show, Eq)

instance ToJSON LogEntry where
  toJSON entry = object
    [ "timestamp" .= entry.logTimestamp
    , "level" .= renderLogLevel entry.logLevel
    , "event" .= entry.logEvent
    , "message" .= entry.logMessage
    ]

type Logger = LogEntry -> IO ()

defaultStderrLogger :: Logger
defaultStderrLogger entry = BL8.hPutStrLn stderr (encode entry)

emitLog :: Logger -> LogLevel -> Text -> Text -> IO ()
emitLog logger level event msg = do
  now <- getCurrentTime
  logger LogEntry
    { logTimestamp = now
    , logLevel = level
    , logEvent = event
    , logMessage = msg
    }

data StoreError = StoreUnavailable | StoreFailure
  deriving (Show, Eq)

data MixtapeStore = MixtapeStore
  { storeCreateMixtape :: Mixtape -> IO (Either StoreError StoredMixtape)
  , storeGetMixtape :: MixtapeId -> IO (Either StoreError (Maybe StoredMixtape))
  }

data AppEnv = AppEnv
  { envStore     :: MixtapeStore
  , envStaticDir :: FilePath
  , envLogger    :: Logger
  }

type AppM = ReaderT AppEnv Handler

postgresMixtapeStore :: Pool.Pool -> MixtapeStore
postgresMixtapeStore = postgresMixtapeStoreWithLogger defaultStderrLogger

postgresMixtapeStoreWithLogger :: Logger -> Pool.Pool -> MixtapeStore
postgresMixtapeStoreWithLogger logger pool = MixtapeStore
  { storeCreateMixtape = \mixtape -> runDb logger "create mixtape" (createMixtape pool mixtape)
  , storeGetMixtape = \tapeId -> runDb logger "get mixtape" (getMixtape pool tapeId)
  }

runDb :: Logger -> String -> IO (Either Pool.UsageError a) -> IO (Either StoreError a)
runDb logger operation action = do
  outcome <- tryJust synchronousException action
  case outcome of
    Left exception -> do
      emitLog logger LogError "database_error" (T.pack ("Database " ++ operation ++ " failed: " ++ show exception))
      pure (Left StoreFailure)
    Right result -> do
      case result of
        Left dbError -> emitLog logger LogError "database_error" (T.pack ("Database " ++ operation ++ " failed: " ++ show dbError))
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
appForStoreAt = appForStoreWithLogger (\_ -> pure ())

appForStoreWithLogger :: Logger -> MixtapeStore -> FilePath -> Application
appForStoreWithLogger logger store directory =
  requestLoggerMiddleware logger (securityHeadersMiddleware (bodySizeLimit (1024 * 1024) (cacheControlMiddleware app)))
  where
    env = AppEnv { envStore = store, envStaticDir = directory, envLogger = logger }
    app = serve appApi (server env)
    csp = "default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'self'; script-src 'self' https://www.youtube.com https://s.ytimg.com; style-src 'self' https://fonts.googleapis.com; font-src 'self' https://fonts.gstatic.com; img-src 'self' data: https://i.ytimg.com; frame-src https://www.youtube.com https://www.youtube-nocookie.com; connect-src 'self' https://www.youtube.com"
    securityHeaders =
      [ ("Content-Security-Policy", csp)
      , ("Referrer-Policy", "strict-origin-when-cross-origin")
      , ("X-Content-Type-Options", "nosniff")
      , ("X-Frame-Options", "SAMEORIGIN")
      ]
    withSecurityHeaders = mapResponseHeaders (securityHeaders ++)
    securityHeadersMiddleware innerApp req sendResp =
      innerApp req (sendResp . withSecurityHeaders)
    cacheControlMiddleware innerApp req sendResp =
      innerApp req $ \resp ->
        case pathInfo req of
          ("assets" : _) -> sendResp (mapResponseHeaders ((hCacheControl, "public, max-age=86400") :) resp)
          _ -> sendResp resp

requestLoggerMiddleware :: Logger -> Application -> Application
requestLoggerMiddleware logger innerApp req sendResp =
  innerApp req $ \resp -> do
    let code = statusCode (responseStatus resp)
        level
          | code >= 500 = LogError
          | code >= 400 = LogWarn
          | otherwise   = LogInfo
        methodText = TE.decodeUtf8Lenient (requestMethod req)
        pathText = TE.decodeUtf8Lenient (rawPathInfo req)
    emitLog logger level "http_request" (methodText <> " " <> pathText <> " " <> T.pack (show code))
    sendResp resp

bodySizeLimit :: Int -> Application -> Application
bodySizeLimit limit app request respond
  | requestMethod request `notElem` ["POST", "PUT", "PATCH"] = app request respond
  | otherwise = do
      body <- collect 0 []
      case body of
        Nothing -> respond $ responseLBS
          status413
          [(hContentType, "text/plain; charset=utf-8")]
          "Request body too large"
        Just chunks -> do
          bodyRef <- newIORef chunks
          let getChunk = atomicModifyIORef' bodyRef $ \remaining ->
                case remaining of
                  [] -> ([], BS.empty)
                  chunk : rest -> (rest, chunk)
          app (setRequestBodyChunks getChunk request) respond
  where
    collect size chunks = do
      chunk <- getRequestBodyChunk request
      if BS.null chunk
        then pure (Just (reverse chunks))
        else if size + BS.length chunk > limit
          then pure Nothing
          else collect (size + BS.length chunk) (chunk : chunks)

server :: AppEnv -> Server AppApi
server env = hoistServer appApi (`runReaderT` env) (serverT env.envStaticDir)

serverT :: FilePath -> ServerT AppApi AppM
serverT staticDirectory =
  lift healthHandler
    :<|> (createMixtapeHandler :<|> getMixtapeHandler)
    :<|> (pageHandler :<|> ((createPageHandler :<|> submitCreateFormHandler) :<|> (createdPageHandler :<|> getSharedMixtapeHandler)))
    :<|> serveDirectoryWebApp staticDirectory

healthHandler :: Handler String
healthHandler = return "https://www.youtube.com/watch?v=_rVvjslF6M8"

pageHandler :: AppM (Html ())
pageHandler = pure renderLandingPage

createPageHandler :: AppM (Html ())
createPageHandler = pure (renderCreatePage Nothing Nothing)

submitCreateFormHandler :: CreateMixtapeForm -> AppM (Html ())
submitCreateFormHandler form = case createMixtapeFromForm form of
  Left validationError -> throwError (htmlError err422 (renderCreatePage (Just validationError) (Just form)))
  Right mixtape -> do
    store <- asks (.envStore)
    result <- liftIO (storeCreateMixtape store mixtape)
    case result of
      Left storeError ->
        let baseError = case storeError of
              StoreUnavailable -> err503
              StoreFailure -> err500
        in throwError (htmlError baseError (renderCreatePage (Just "We could not save your mixtape. Please try again.") (Just form)))
      Right created -> do
        let target = "/created/" <> toUrlPiece (shareCodeFor created.mixtapeId)
            targetBs = TE.encodeUtf8 target
        throwError err303
          { errHeaders =
              [ (hLocation, targetBs)
              , ("HX-Redirect", targetBs)
              ]
          }

createdPageHandler :: ShareCode -> AppM (Html ())
createdPageHandler shareCode = do
  store <- asks (.envStore)
  result <- liftIO (storeGetMixtape store (mixtapeIdFromShareCode shareCode))
  case result of
    Left storeError -> throwError (htmlStoreErrorResponse storeError)
    Right Nothing -> throwError htmlNotFoundError
    Right (Just mixtape) -> pure (renderCreatedPage mixtape)

createMixtapeHandler
  :: Mixtape
  -> AppM (Headers '[Header "Location" Text] StoredMixtape)
createMixtapeHandler mixtape = do
  store <- asks (.envStore)
  result <- liftIO (storeCreateMixtape store mixtape)
  case result of
    Left storeError -> do
      throwError (storeErrorResponse storeError)
    Right created -> pure $ addHeader
      ("/m/" <> toUrlPiece (shareCodeFor created.mixtapeId))
      created

getMixtapeHandler :: MixtapeId -> AppM StoredMixtape
getMixtapeHandler tapeId = do
  store <- asks (.envStore)
  result <- liftIO (storeGetMixtape store tapeId)
  case result of
    Left storeError -> do
      throwError (storeErrorResponse storeError)
    Right Nothing -> throwError err404
    Right (Just mixtape) -> pure mixtape

getSharedMixtapeHandler :: ShareCode -> AppM (Html ())
getSharedMixtapeHandler shareCode = do
  store <- asks (.envStore)
  result <- liftIO (storeGetMixtape store (mixtapeIdFromShareCode shareCode))
  case result of
    Left storeError -> throwError (htmlStoreErrorResponse storeError)
    Right Nothing -> throwError htmlNotFoundError
    Right (Just mixtape) -> pure (renderMixtapePage mixtape)

storeErrorResponse :: StoreError -> ServerError
storeErrorResponse storeError =
  case storeError of
    StoreUnavailable -> jsonError err503 "storage_unavailable"
    StoreFailure -> jsonError err500 "internal_error"

htmlStoreErrorResponse :: StoreError -> ServerError
htmlStoreErrorResponse storeError =
  case storeError of
    StoreUnavailable ->
      htmlError err503 (renderErrorPage "Temporarily unavailable" "We could not reach the mixtape archive right now. Please try again in a moment.")
    StoreFailure ->
      htmlError err500 (renderErrorPage "Something went wrong" "We could not load this mixtape right now. Please try again.")

htmlNotFoundError :: ServerError
htmlNotFoundError =
  htmlError err404 (renderErrorPage "Mixtape not found" "We could not find a mixtape at this link.")

htmlError :: ServerError -> Html () -> ServerError
htmlError response page = response
  { errBody = renderBS page
  , errHeaders = (hContentType, "text/html; charset=utf-8") : errHeaders response
  }

jsonError :: ServerError -> String -> ServerError
jsonError response errorCode = response
  { errBody = encode (object ["error" .= errorCode])
  , errHeaders = (hContentType, "application/json") : errHeaders response
  }
