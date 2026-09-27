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

    it "extracts ID from standard watch URL" $ do
      let res = mkYouTubeVideoId "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from watch URL with extra query parameters" $ do
      let res = mkYouTubeVideoId "https://www.youtube.com/watch?feature=share&v=dQw4w9WgXcQ&t=42s"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from watch URL with subsequent parameters containing 'v='" $ do
      let res1 = mkYouTubeVideoId "https://www.youtube.com/watch?v=dQw4w9WgXcQ&prev=true"
      fmap unYouTubeVideoId res1 `shouldBe` Right "dQw4w9WgXcQ"

      let res2 = mkYouTubeVideoId "https://www.youtube.com/watch?v=dQw4w9WgXcQ&nav=home"
      fmap unYouTubeVideoId res2 `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from youtu.be short URL" $ do
      let res = mkYouTubeVideoId "https://youtu.be/dQw4w9WgXcQ?si=abc"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from youtu.be URL without scheme" $ do
      let res = mkYouTubeVideoId "youtu.be/dQw4w9WgXcQ"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from shorts URL" $ do
      let res = mkYouTubeVideoId "https://www.youtube.com/shorts/dQw4w9WgXcQ"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "extracts ID from embed URL" $ do
      let res = mkYouTubeVideoId "https://www.youtube.com/embed/dQw4w9WgXcQ"
      fmap unYouTubeVideoId res `shouldBe` Right "dQw4w9WgXcQ"

    it "rejects a video-like URL on an unrelated host" $ do
      mkYouTubeVideoId "https://example.com/watch?v=dQw4w9WgXcQ"
        `shouldSatisfy` either (const True) (const False)

    it "rejects malformed UTF-8 in a URL query" $ do
      mkYouTubeVideoId "https://www.youtube.com/watch?v=%FF1234567890"
        `shouldBe` Left (InvalidCharacters "\xfffd")

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

    it "decodes URL to normalized ID" $ do
      let res = decode "\"https://youtu.be/dQw4w9WgXcQ\""
      fmap unYouTubeVideoId res `shouldBe` Just "dQw4w9WgXcQ"

    it "fails decoding invalid JSON string for YouTubeVideoId" $ do
      let res = decode "\"not-valid\"" :: Maybe YouTubeVideoId
      res `shouldBe` Nothing

    it "fails decoding invalid ID in Track record" $ do
      let res = decode "{\"videoId\":\"not-valid\",\"note\":\"test\"}" :: Maybe Track
      res `shouldBe` Nothing
