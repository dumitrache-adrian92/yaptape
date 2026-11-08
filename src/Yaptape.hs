{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape (run, runWithStatic, withApp, withAppForDb) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  , Port
  )
import Control.Exception (bracket)
import System.IO (hPutStrLn, stderr)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Yaptape.Db.Postgres (DbConfig, createPoolFromConfig, resolveDbConfig, runMigrations)
import Yaptape.Server (appForStore, appForStoreAt, postgresMixtapeStore)
import qualified Hasql.Pool as Pool

run :: IO ()
run = runWithStatic "static"

withPool :: DbConfig -> (Pool.Pool -> IO a) -> IO a
withPool dbCfg = bracket (createPoolFromConfig dbCfg) Pool.release

data Config = Config
  { cfgPort       :: Port
  , cfgStaticDir  :: FilePath
  , cfgDb         :: DbConfig
  }

resolveConfig :: FilePath -> IO Config
resolveConfig bundledStatic = do
  portEnv   <- lookupEnv "PORT"
  staticEnv <- lookupEnv "STATIC_DIR"
  port <- either (ioError . userError) pure (parsePort portEnv)
  dbCfg <- resolveDbConfig
  pure Config
    { cfgPort      = port
    , cfgStaticDir = fromMaybe bundledStatic staticEnv
    , cfgDb        = dbCfg
    }

parsePort :: Maybe String -> Either String Port
parsePort Nothing    = Right 3000
parsePort (Just raw) = case readMaybe raw of
  Nothing -> Left "PORT must be an integer"
  Just n
    | n > 0 && n <= 65535 -> Right n
    | otherwise           -> Left "PORT must be between 1 and 65535"

ensureMigrations :: Pool.Pool -> IO ()
ensureMigrations pool = do
  migResult <- runMigrations pool
  case migResult of
    Left err -> ioError (userError ("Database migration failed: " ++ show err))
    Right () -> pure ()

runWithStatic :: FilePath -> IO ()
runWithStatic bundledStatic = do
  cfg <- resolveConfig bundledStatic
  let settings =
        setPort cfg.cfgPort $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show cfg.cfgPort)) $
        defaultSettings
  withPool cfg.cfgDb $ \pool -> do
    ensureMigrations pool
    runSettings settings (appForStoreAt (postgresMixtapeStore pool) cfg.cfgStaticDir)

withApp :: (Application -> IO a) -> IO a
withApp action = do
  dbCfg <- resolveDbConfig
  withAppForDb dbCfg action

withAppForDb :: DbConfig -> (Application -> IO a) -> IO a
withAppForDb dbCfg action =
  withPool dbCfg $ \pool -> do
    ensureMigrations pool
    action (appForStore (postgresMixtapeStore pool))
