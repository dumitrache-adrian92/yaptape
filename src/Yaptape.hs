{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape (run, runWithStatic, runWithPaths, withApp, withAppForDb) where

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

import Yaptape.Db.Postgres (DbConfig, createPoolFromConfig, resolveDbConfig, runMigrations, runMigrationsFrom)
import Yaptape.Pages (configureAssetVersion)
import Yaptape.Server (appForStore, appForStoreWithLogger, defaultStderrLogger, postgresMixtapeStore)
import qualified Hasql.Pool as Pool

run :: IO ()
run = runWithStatic "static"

withPool :: DbConfig -> (Pool.Pool -> IO a) -> IO a
withPool dbCfg = bracket (createPoolFromConfig dbCfg) Pool.release

data Config = Config
  { cfgPort          :: Port
  , cfgStaticDir     :: FilePath
  , cfgMigrationsDir :: FilePath
  , cfgDb            :: DbConfig
  }

resolveConfig :: FilePath -> FilePath -> IO Config
resolveConfig bundledStatic bundledMigrations = do
  portEnv       <- lookupEnv "PORT"
  staticEnv     <- lookupEnv "STATIC_DIR"
  migrationsEnv <- lookupEnv "MIGRATIONS_DIR"
  port <- either (ioError . userError) pure (parsePort portEnv)
  dbCfg <- resolveDbConfig
  pure Config
    { cfgPort          = port
    , cfgStaticDir     = fromMaybe bundledStatic staticEnv
    , cfgMigrationsDir = fromMaybe bundledMigrations migrationsEnv
    , cfgDb            = dbCfg
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

ensureMigrationsFrom :: Pool.Pool -> FilePath -> IO ()
ensureMigrationsFrom pool migrationsDir = do
  migResult <- runMigrationsFrom pool migrationsDir
  case migResult of
    Left err -> ioError (userError ("Database migration failed: " ++ show err))
    Right () -> pure ()

runWithStatic :: FilePath -> IO ()
runWithStatic bundledStatic = runWithPaths bundledStatic "db/migrations"

runWithPaths :: FilePath -> FilePath -> IO ()
runWithPaths bundledStatic bundledMigrations = do
  cfg <- resolveConfig bundledStatic bundledMigrations
  configureAssetVersion cfg.cfgStaticDir
  let settings =
        setPort cfg.cfgPort $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show cfg.cfgPort)) $
        defaultSettings
  withPool cfg.cfgDb $ \pool -> do
    ensureMigrationsFrom pool cfg.cfgMigrationsDir
    runSettings settings (appForStoreWithLogger defaultStderrLogger (postgresMixtapeStore pool) cfg.cfgStaticDir)

withApp :: (Application -> IO a) -> IO a
withApp action = do
  dbCfg <- resolveDbConfig
  withAppForDb dbCfg action

withAppForDb :: DbConfig -> (Application -> IO a) -> IO a
withAppForDb dbCfg action =
  withPool dbCfg $ \pool -> do
    ensureMigrations pool
    action (appForStore (postgresMixtapeStore pool))
