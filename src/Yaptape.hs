module Yaptape (run) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setBeforeMainLoop
  , setPort
  )
import Servant (serve)
import System.IO (hPutStrLn, stderr)

import Yaptape.Api (appApi)
import Yaptape.Server (server)

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve appApi server
