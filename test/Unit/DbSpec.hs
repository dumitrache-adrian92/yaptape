{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.DbSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian)
import Test.Hspec

import Yaptape.Db
  ( MixtapeRow (..)
  , TrackRow (..)
  , fromDbRows
  , toTrackRows
  )
import Yaptape.Domain
  ( MixtapeId (..)
  , StoredMixtape (..)
  , StoredTrack (..)
  , Track (..)
  , TrackId (..)
  )
import Yaptape.YouTube (mkYouTubeVideoId)

spec :: Spec
spec = describe "Yaptape.Db" $ do
  let mId = MixtapeId "mixtape-123"
      time = UTCTime (fromGregorian 2026 9 27) 0
      vid1 = either (error . show) id (mkYouTubeVideoId "dQw4w9WgXcQ")
      vid2 = either (error . show) id (mkYouTubeVideoId "_rVvjslF6M8")

      domainTrack1 = Track vid1 (Just "Track 1") (Just "Artist 1") "Note 1"
      domainTrack2 = Track vid2 Nothing Nothing "Note 2"

      storedTrack1 = StoredTrack (TrackId "t-1") vid1 (Just "Track 1") (Just "Artist 1") "Note 1"
      storedTrack2 = StoredTrack (TrackId "t-2") vid2 Nothing Nothing "Note 2"

      serverMixtape = StoredMixtape
        { mixtapeId = mId
        , title = "My Tape"
        , description = Just "Cool vibes"
        , createdAt = time
        , tracks = [storedTrack1, storedTrack2]
        }

  describe "toTrackRows" $ do
    it "converts domain tracks to relational track rows with correct order and mixtapeId" $ do
      let tRows = toTrackRows mId (\idx -> TrackId ("t-" <> (if idx == 0 then "1" else "2"))) [domainTrack1, domainTrack2]
      case tRows of
        [r1, r2] -> do
          r1.trackId `shouldBe` TrackId "t-1"
          r1.mixtapeId `shouldBe` mId
          r1.trackOrder `shouldBe` 0
          r1.videoId `shouldBe` vid1

          r2.trackId `shouldBe` TrackId "t-2"
          r2.mixtapeId `shouldBe` mId
          r2.trackOrder `shouldBe` 1
          r2.videoId `shouldBe` vid2
        _ -> expectationFailure "Expected exactly 2 track rows"

  describe "fromDbRows" $ do
    it "reconstructs StoredMixtape from MixtapeRow and TrackRows preserving order" $ do
      let mRow = MixtapeRow mId "My Tape" (Just "Cool vibes") time
          r1 = TrackRow (TrackId "t-1") mId 0 vid1 (Just "Track 1") (Just "Artist 1") "Note 1"
          r2 = TrackRow (TrackId "t-2") mId 1 vid2 Nothing Nothing "Note 2"

      -- Pass in reverse order to ensure it sorts by trackOrder
      let reconstructed = fromDbRows mRow [r2, r1]
      reconstructed `shouldBe` serverMixtape
