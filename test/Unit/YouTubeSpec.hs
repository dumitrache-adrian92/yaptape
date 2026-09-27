{-# LANGUAGE OverloadedStrings #-}

module Unit.YouTubeSpec (spec) where

import Data.Aeson (decode)
import Test.Hspec
import Yaptape.Domain (Track (..))
import Yaptape.YouTube
  ( YouTubeVideoId
  , YouTubeVideoIdError (..)
  , mkYouTubeVideoId
  , unYouTubeVideoId
  )

spec :: Spec
spec = describe "Yaptape.YouTube" $ do
  describe "YouTubeVideoId" $ do
    it "accepts valid 11-character base64url IDs" $ do
      let res = mkYouTubeVideoId "dQw4w9WgXcQ"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "accepts IDs containing underscores and hyphens" $ do
      let res = mkYouTubeVideoId "_rVvjslF-M8"
      fmap unYouTubeVideoId res `shouldBe` Right "_rVvjslF-M8"

    it "rejects URLs; callers provide the video ID explicitly" $ do
      mkYouTubeVideoId "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
        `shouldSatisfy` either (const True) (const False)

    it "rejects a video-like URL on an unrelated host" $ do
      mkYouTubeVideoId "https://example.com/watch?v=dQw4w9WgXcQ"
        `shouldSatisfy` either (const True) (const False)

    it "rejects URL input instead of decoding its query string" $ do
      mkYouTubeVideoId "https://www.youtube.com/watch?v=%FF1234567890"
        `shouldSatisfy` either (const True) (const False)

    it "fails on empty string" $ do
      mkYouTubeVideoId "" `shouldBe` Left EmptyInput
      mkYouTubeVideoId "   " `shouldBe` Left EmptyInput

    it "fails on invalid length" $ do
      mkYouTubeVideoId "tooShort" `shouldBe` Left (InvalidLength 8)
      mkYouTubeVideoId "thisIsWayTooLongForAnId" `shouldBe` Left (InvalidLength 23)

    it "fails on invalid characters" $ do
      mkYouTubeVideoId "dQw4w9!gXcQ" `shouldBe` Left (InvalidCharacters "!")

    it "fails on non-ASCII unicode characters" $ do
      mkYouTubeVideoId "dQw4w9αgXcQ" `shouldBe` Left (InvalidCharacters "α")

  describe "Aeson JSON parsing for YouTubeVideoId" $ do
    it "decodes valid JSON string" $ do
      let res = decode "\"dQw4w9WgXcQ\""
      fmap unYouTubeVideoId res `shouldBe` Just "dQw4w9WgXcQ"

    it "rejects URL input during JSON decoding" $ do
      let res = decode "\"https://youtu.be/dQw4w9WgXcQ\""
      (res :: Maybe YouTubeVideoId) `shouldBe` Nothing

    it "fails decoding invalid JSON string for YouTubeVideoId" $ do
      let res = decode "\"not-valid\"" :: Maybe YouTubeVideoId
      res `shouldBe` Nothing

    it "fails decoding invalid ID in Track record" $ do
      let res = decode "{\"videoId\":\"not-valid\",\"note\":\"test\"}" :: Maybe Track
      res `shouldBe` Nothing
