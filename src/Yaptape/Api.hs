{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Yaptape.Api
  ( AppApi
  , appApi
  , HealthApi
  , healthApi
  , MixtapeApi
  , mixtapeApi
  , getMixtapeApi
  , PagesApi
  , pagesApi
  , MixtapePageApi
  , mixtapePageApi
  ) where

import Data.Proxy (Proxy (..))
import Lucid (Html)
import Data.Text (Text)
import Servant.API ((:<|>) (..), (:>), Capture, FormUrlEncoded, Get, Header, Headers, JSON, PlainText, Post, PostCreated, Raw, ReqBody)
import Servant.HTML.Lucid (HTML)
import Yaptape.Domain (Mixtape, MixtapeId, ShareCode, StoredMixtape)
import Yaptape.Form (CreateMixtapeForm)

type HealthApi = "health" :> Get '[PlainText] String

type MixtapeApi = "mixtapes" :> (CreateMixtapeApi :<|> GetMixtapeApi)

type CreateMixtapeApi = ReqBody '[JSON] Mixtape :> PostCreated '[JSON] (Headers '[Header "Location" Text] StoredMixtape)

type GetMixtapeApi = Capture "mixtapeId" MixtapeId :> Get '[JSON] StoredMixtape

type PagesApi = Get '[HTML] (Html ())

type CreatePageApi = "create" :> (Get '[HTML] (Html ()) :<|> (ReqBody '[FormUrlEncoded] CreateMixtapeForm :> Post '[HTML] (Html ())))

type MixtapePageApi = "m" :> Capture "shareCode" ShareCode :> Get '[HTML] (Html ())

type AppApi = HealthApi :<|> ("api" :> MixtapeApi) :<|> (PagesApi :<|> (CreatePageApi :<|> MixtapePageApi)) :<|> ("assets" :> Raw)

appApi :: Proxy AppApi
appApi = Proxy

healthApi :: Proxy HealthApi
healthApi = Proxy

mixtapeApi :: Proxy ("api" :> "mixtapes" :> CreateMixtapeApi)
mixtapeApi = Proxy

getMixtapeApi :: Proxy ("api" :> "mixtapes" :> GetMixtapeApi)
getMixtapeApi = Proxy

pagesApi :: Proxy PagesApi
pagesApi = Proxy

mixtapePageApi :: Proxy MixtapePageApi
mixtapePageApi = Proxy
