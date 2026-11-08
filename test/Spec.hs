{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Test.Hspec
import qualified Integration.DbSpec
import qualified Integration.HealthSpec
import qualified Integration.MixtapeSpec
import Integration.TestDb (TestEnv (..), withTestEnv)
import qualified Unit.DbSpec
import qualified Unit.ApiSpec
import qualified Unit.HealthSpec
import qualified Unit.YouTubeSpec

main :: IO ()
main = hspec $ do
  describe "Unit Tests" $ do
    Unit.HealthSpec.spec
    Unit.YouTubeSpec.spec
    Unit.DbSpec.spec
    Unit.ApiSpec.spec
  aroundAll withTestEnv $ describe "Integration Tests" $ do
    beforeWith (\env -> pure env.testApp) $ do
      Integration.HealthSpec.spec
      Integration.MixtapeSpec.spec
    beforeWith (\env -> pure env.testPool) $ do
      Integration.DbSpec.spec
