{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Yaptape.Api
  ( AppApi
  , appApi
  , HealthApi
  , healthApi
  ) where

import Data.Proxy (Proxy (..))
import Servant.API ((:>), Get, PlainText)

type HealthApi = "health" :> Get '[PlainText] String

type AppApi = HealthApi

appApi :: Proxy AppApi
appApi = Proxy

healthApi :: Proxy HealthApi
healthApi = Proxy
