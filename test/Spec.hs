module Main (main) where

import Test.Hspec
import qualified Integration.HealthSpec
import qualified Unit.DbSpec
import qualified Unit.HealthSpec
import qualified Unit.YouTubeSpec

main :: IO ()
main = hspec $ do
  describe "Unit Tests" $ do
    Unit.HealthSpec.spec
    Unit.YouTubeSpec.spec
    Unit.DbSpec.spec
  describe "Integration Tests" Integration.HealthSpec.spec
