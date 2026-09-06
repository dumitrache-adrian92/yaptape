module Main (main) where

import Test.Hspec
import qualified Integration.HealthSpec
import qualified Unit.HealthSpec

main :: IO ()
main = hspec $ do
  describe "Unit Tests" Unit.HealthSpec.spec
  describe "Integration Tests" Integration.HealthSpec.spec
