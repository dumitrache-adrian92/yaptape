module Yaptape (run, runWithStatic, withApp) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
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

runWithStatic :: FilePath -> IO ()
runWithStatic bundledStatic = do
  portEnv <- lookupEnv "PORT"
  staticEnv <- lookupEnv "STATIC_DIR"
  port <- case portEnv of
    Nothing -> pure 3000
    Just raw -> case readMaybe raw of
      Just value | value > 0 && value <= 65535 -> pure value
      _ -> ioError (userError "PORT must be an integer between 1 and 65535")
  let staticDirectory = fromMaybe bundledStatic staticEnv
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  bracket createPool Pool.release $ \pool ->
    runSettings settings (appForStoreAt (postgresMixtapeStore pool) staticDirectory)

withApp :: (Application -> IO a) -> IO a
withApp action = bracket createPool Pool.release $ \pool ->
  action (appForStore (postgresMixtapeStore pool))
