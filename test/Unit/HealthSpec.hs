module Unit.HealthSpec (spec) where

import Servant (runHandler)
import Test.Hspec
import Yaptape.Server (healthHandler)

spec :: Spec
spec = describe "Health Handler (Unit)" $ do
  it "returns health message directly from handler" $ do
    res <- runHandler healthHandler
    res `shouldBe` Right "https://www.youtube.com/watch?v=_rVvjslF6M8"
