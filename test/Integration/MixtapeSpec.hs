{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Integration.MixtapeSpec (spec) where

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
import Data.Text (Text)
import Servant.API (Header, Headers, getResponse)
import Yaptape (withApp)
import Yaptape.Api (getMixtapeApi, mixtapeApi)
import Yaptape.Domain
  ( Mixtape (..)
  , MixtapeId
  , StoredMixtape (..)
  , StoredTrack (..)
  , Track (..)
  , videoId
  , unMixtapeId
  , unTrackId
  )
import Yaptape.YouTube (mkYouTubeVideoId)

postMixtape :: Mixtape -> ClientM (Headers '[Header "Location" Text] StoredMixtape)
postMixtape = client mixtapeApi

getMixtape :: MixtapeId -> ClientM StoredMixtape
getMixtape = client getMixtapeApi

spec :: Spec
spec = describe "POST /mixtape (Integration)" $ do
  it "persists a mixtape and its tracks and returns generated identifiers" $ do
    let youtubeVideo = either (error . show) id (mkYouTubeVideoId "dQw4w9WgXcQ")
        mixtape = Mixtape
          { title = "Integration tape"
          , description = Just "Created by the API integration test"
          , tracks = [Track youtubeVideo (Just "Track title") (Just "Artist") "A note"]
          }
    withApp $ \app -> testWithApplication (pure app) $ \port -> do
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
            [track] -> do
              unTrackId (trackId track) `shouldNotBe` ""
              track.videoId `shouldBe` youtubeVideo
              track.title `shouldBe` Just "Track title"
              track.artist `shouldBe` Just "Artist"
              track.note `shouldBe` "A note"
            _ -> expectationFailure "Expected one persisted track"
          fetchedResult <- runClientM (getMixtape created.mixtapeId) env
          fetchedResult `shouldBe` Right created
