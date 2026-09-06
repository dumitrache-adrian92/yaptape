module Yaptape.Server
  ( server
  , healthHandler
  ) where

import Servant (Handler, Server)
import Yaptape.Api (AppApi)

server :: Server AppApi
server = healthHandler

healthHandler :: Handler String
healthHandler = return "https://www.youtube.com/watch?v=_rVvjslF6M8"
