module Integration.HealthSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
  ( BaseUrl (..)
  , ClientM
  , Scheme (Http)
  , client
  , mkClientEnv
  , runClientM
  )
import Test.Hspec
import Yaptape (withApp)
import Yaptape.Api (healthApi)

getHealth :: ClientM String
getHealth = client healthApi

spec :: Spec
spec = describe "GET /health (Integration)" $ do
  it "returns 200 OK with health message via HTTP client" $ do
    withApp $ \app -> testWithApplication (pure app) $ \port -> do
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      res <- runClientM getHealth env
      res `shouldBe` Right "https://www.youtube.com/watch?v=_rVvjslF6M8"
