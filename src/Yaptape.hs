module Yaptape (run, withApp) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Control.Exception (bracket)
import System.IO (hPutStrLn, stderr)

import Yaptape.Db.Postgres (createPool)
import Yaptape.Server (appForStore, postgresMixtapeStore)
import qualified Hasql.Pool as Pool

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  bracket createPool Pool.release $ \pool ->
    runSettings settings (appForStore (postgresMixtapeStore pool))

withApp :: (Application -> IO a) -> IO a
withApp action = bracket createPool Pool.release $ \pool ->
  action (appForStore (postgresMixtapeStore pool))
