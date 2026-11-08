{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Integration.MixtapeSpec (spec) where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status404)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client
  ( BaseUrl (..)
  , ClientError (FailureResponse)
  , ClientM
  , ResponseF (responseStatusCode)
  , Scheme (Http)
  , client
  , mkClientEnv
  , runClientM
  )
import Test.Hspec
import Data.Text (Text)
import Servant.API (Header, Headers, getResponse)
import Yaptape.Api (getMixtapeApi, mixtapeApi)
import Yaptape.Domain
  ( Mixtape
  , MixtapeId
  , StoredMixtape (..)
  , StoredTrack (..)
  , mkMixtape
  , mkMixtapeId
  , mkTrack
  , videoId
  , unMixtapeId
  , unTrackId
  )
import Yaptape.YouTube (mkYouTubeVideoId)

postMixtape :: Mixtape -> ClientM (Headers '[Header "Location" Text] StoredMixtape)
postMixtape = client mixtapeApi

getMixtape :: MixtapeId -> ClientM StoredMixtape
getMixtape = client getMixtapeApi

spec :: SpecWith Application
spec = describe "POST & GET /api/mixtapes (Integration)" $ do
  it "persists a multi-track mixtape in order and returns generated identifiers" $ \app -> do
    let vid1 = either (error . show) id (mkYouTubeVideoId "dQw4w9WgXcQ")
        vid2 = either (error . show) id (mkYouTubeVideoId "_rVvjslF6M8")
        vid3 = either (error . show) id (mkYouTubeVideoId "M7lc1UVf-VE")
        trk1 = either (error . show) id (mkTrack vid1 (Just "First track") (Just "Artist 1") "Opening note")
        trk2 = either (error . show) id (mkTrack vid2 Nothing Nothing "Middle note")
        trk3 = either (error . show) id (mkTrack vid3 (Just "Final track") Nothing "Closing note")
        mixtape = either (error . show) id $ mkMixtape
          "Integration tape"
          (Just "Created by the API integration test")
          [trk1, trk2, trk3]
    testWithApplication (pure app) $ \port -> do
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      result <- runClientM (postMixtape mixtape) env
      case result of
        Left err -> expectationFailure (show err)
        Right response -> do
          let created = getResponse response
          created.title `shouldBe` "Integration tape"
          created.description `shouldBe` Just "Created by the API integration test"
          unMixtapeId created.mixtapeId `shouldNotBe` ""
          case created.tracks of
            [t1, t2, t3] -> do
              unTrackId (trackId t1) `shouldNotBe` ""
              t1.videoId `shouldBe` vid1
              t1.title `shouldBe` Just "First track"
              t1.artist `shouldBe` Just "Artist 1"
              t1.note `shouldBe` "Opening note"

              unTrackId (trackId t2) `shouldNotBe` ""
              t2.videoId `shouldBe` vid2
              t2.title `shouldBe` Nothing
              t2.artist `shouldBe` Nothing
              t2.note `shouldBe` "Middle note"

              unTrackId (trackId t3) `shouldNotBe` ""
              t3.videoId `shouldBe` vid3
              t3.title `shouldBe` Just "Final track"
              t3.artist `shouldBe` Nothing
              t3.note `shouldBe` "Closing note"
            _ -> expectationFailure "Expected three persisted tracks in order"
          fetchedResult <- runClientM (getMixtape created.mixtapeId) env
          fetchedResult `shouldBe` Right created

  it "returns 404 Not Found for an unknown mixtape UUID" $ \app -> do
    let missingId = maybe (error "invalid UUID") id (mkMixtapeId "00000000-0000-4000-8000-000000000099")
    testWithApplication (pure app) $ \port -> do
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      fetchedResult <- runClientM (getMixtape missingId) env
      case fetchedResult of
        Left (FailureResponse _ resp) -> responseStatusCode resp `shouldBe` status404
        other -> expectationFailure ("Expected 404 FailureResponse, got: " ++ show other)
