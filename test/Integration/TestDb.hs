{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Integration.TestDb
  ( TestEnv (..)
  , withTestApp
  , withTestEnv
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, onException)
import Data.Char (isDigit)
import Data.Word (Word16)
import Network.Wai (Application)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Yaptape.Db.Postgres (DbConfig (..), createPoolFromConfig, runMigrations)
import Yaptape.Server (appForStore, postgresMixtapeStore)
import qualified Hasql.Connection as Connection
import Hasql.Connection.Setting (connection)
import Hasql.Connection.Setting.Connection (params)
import qualified Hasql.Connection.Setting.Connection.Param as Param
import qualified Hasql.Pool as Pool

data TestEnv = TestEnv
  { testApp  :: Application
  , testPool :: Pool.Pool
  }

-- | Spin up an ephemeral @postgres:16@ Docker container in RAM (@--tmpfs@)
-- bound to a random localhost port, wait for readiness and apply migrations,
-- run the test action with a configured 'TestEnv', and remove the container.
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action =
  bracket startPostgresContainer stopPostgresContainer $ \(_, dbCfg) -> do
    waitForPostgres dbCfg 50
    bracket (createPoolFromConfig dbCfg) Pool.release $ \pool -> do
      migResult <- runMigrations pool
      case migResult of
        Left err -> ioError (userError ("Database migration failed: " ++ show err))
        Right () -> action TestEnv
          { testApp = appForStore (postgresMixtapeStore pool)
          , testPool = pool
          }

withTestApp :: (Application -> IO a) -> IO a
withTestApp action = withTestEnv (\env -> action env.testApp)

startPostgresContainer :: IO (String, DbConfig)
startPostgresContainer = do
  rawId <- readProcess "docker"
    [ "run"
    , "-d"
    , "--rm"
    , "--tmpfs", "/var/lib/postgresql/data"
    , "-e", "POSTGRES_DB=yaptape_test"
    , "-e", "POSTGRES_USER=yaptape"
    , "-e", "POSTGRES_PASSWORD=yaptape-test"
    , "-p", "127.0.0.1:0:5432"
    , "postgres:16"
    , "-c", "fsync=off"
    , "-c", "full_page_writes=off"
    ]
    ""
  let containerId = filter (/= '\n') rawId
  (do
    portOut <- readProcess "docker" ["port", containerId, "5432/tcp"] ""
    port <- parseMappedPort portOut
    let dbCfg = DbConfig
          { dbHost = "127.0.0.1"
          , dbPort = port
          , dbUser = "yaptape"
          , dbPassword = "yaptape-test"
          , dbName = "yaptape_test"
          }
    pure (containerId, dbCfg)) `onException` removeContainer containerId

stopPostgresContainer :: (String, DbConfig) -> IO ()
stopPostgresContainer (containerId, _) = removeContainer containerId

removeContainer :: String -> IO ()
removeContainer containerId = do
  _ <- readProcess "docker" ["rm", "-f", containerId] ""
  pure ()

parseMappedPort :: String -> IO Word16
parseMappedPort raw =
  case reverse (words raw) of
    (firstLine : _) ->
      let portStr = reverse (takeWhile isDigit (reverse firstLine))
      in case readMaybe portStr of
           Just p  -> pure p
           Nothing -> ioError (userError ("Could not parse mapped docker port from: " ++ raw))
    [] -> ioError (userError "Empty output from docker port")

waitForPostgres :: DbConfig -> Int -> IO ()
waitForPostgres dbCfg retriesLeft
  | retriesLeft <= 0 = ioError (userError "Timed out waiting for ephemeral PostgreSQL container")
  | otherwise = do
      let probeSettings =
            connection $ params
              [ Param.host dbCfg.dbHost
              , Param.port dbCfg.dbPort
              , Param.user dbCfg.dbUser
              , Param.password dbCfg.dbPassword
              , Param.dbname dbCfg.dbName
              ]
      connResult <- Connection.acquire [probeSettings]
      case connResult of
        Right conn -> Connection.release conn
        Left _ -> do
          threadDelay 100000 -- 100ms
          waitForPostgres dbCfg (retriesLeft - 1)
