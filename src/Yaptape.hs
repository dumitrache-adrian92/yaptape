module Yaptape (run, runWithStatic, withApp) where

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

import Yaptape.Db.Postgres (createPool)
import Yaptape.Server (appForStore, appForStoreAt, postgresMixtapeStore)
import qualified Hasql.Pool as Pool

run :: IO ()
run = runWithStatic "static"

withPool :: (Pool.Pool -> IO a) -> IO a
withPool = bracket createPool Pool.release

data Config = Config
  { cfgPort       :: Port
  , cfgStaticDir  :: FilePath
  }

resolveConfig :: FilePath -> IO Config
resolveConfig bundledStatic = do
  portEnv   <- lookupEnv "PORT"
  staticEnv <- lookupEnv "STATIC_DIR"
  port <- either (ioError . userError) pure (parsePort portEnv)
  pure Config
    { cfgPort      = port
    , cfgStaticDir = fromMaybe bundledStatic staticEnv
    }

parsePort :: Maybe String -> Either String Port
parsePort Nothing    = Right 3000
parsePort (Just raw) = case readMaybe raw of
  Nothing -> Left "PORT must be an integer"
  Just n
    | n > 0 && n <= 65535 -> Right n
    | otherwise           -> Left "PORT must be between 1 and 65535"

runWithStatic :: FilePath -> IO ()
runWithStatic bundledStatic = do
  Config{cfgPort = port, cfgStaticDir = staticDir} <- resolveConfig bundledStatic
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  withPool $ \pool ->
    runSettings settings (appForStoreAt (postgresMixtapeStore pool) staticDir)

withApp :: (Application -> IO a) -> IO a
withApp action = withPool (action . appForStore . postgresMixtapeStore)
