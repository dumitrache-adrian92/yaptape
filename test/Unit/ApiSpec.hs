{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Unit.ApiSpec (spec) where

import Data.Time (UTCTime (..), fromGregorian)
import Data.List (isPrefixOf, tails)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (decode)
import Network.HTTP.Client (RequestBody (RequestBodyLBS), defaultManagerSettings, httpLbs, newManager, parseRequest, requestBody, requestHeaders, method, responseBody, responseHeaders, responseStatus)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.HTTP.Types (statusCode)
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
import Servant.API (Header, Headers, getHeaders, getResponse)
import Yaptape.Api (getMixtapeApi, mixtapeApi)
import Yaptape.Domain
  ( Mixtape (..)
  , MixtapeId
  , StoredMixtape (..)
  , StoredTrack (..)
  , ShareCode
  , Track (..)
  , mkMixtape
  , mkMixtapeId
  , mkTrack
  , mkTrackId
  , shareCodeFor
  )
import Servant.API (ToHttpApiData (toUrlPiece))
import Servant.API (FromHttpApiData (parseUrlPiece))
import Yaptape.Server (MixtapeStore (..), StoreError (..), appForStore)
import Yaptape.Form (CreateMixtapeForm (..), createMixtapeFromForm)
import Yaptape.YouTube (mkYouTubeVideoId)
import Web.FormUrlEncoded (urlDecodeAsForm)

postMixtape :: Mixtape -> ClientM (Headers '[Header "Location" Text] StoredMixtape)
postMixtape = client mixtapeApi

getMixtape :: MixtapeId -> ClientM StoredMixtape
getMixtape = client getMixtapeApi

spec :: Spec
spec = describe "Mixtape routes with an in-memory store" $ do
  describe "validated identifiers and creation input" $ do
    it "rejects malformed UUID text for mixtape and track IDs" $ do
      mkMixtapeId "mixtape-1" `shouldBe` Nothing
      mkTrackId "track-1" `shouldBe` Nothing

    it "round trips a compact share code" $ do
      let mixtapeKey = requiredMaybe (mkMixtapeId "00000000-0000-4000-8000-000000000001")
          code = shareCodeFor mixtapeKey
          encoded = toUrlPiece code
      T.length encoded `shouldBe` 22
      (parseUrlPiece encoded :: Either Text ShareCode) `shouldBe` Right code

    it "rejects blank titles, blank notes, and tapes without tracks" $ do
      let blankTitle = decode "{\"title\":\"  \",\"tracks\":[{\"videoId\":\"dQw4w9WgXcQ\",\"note\":\"n\"}]}" :: Maybe Mixtape
          blankNote = decode "{\"title\":\"Tape\",\"tracks\":[{\"videoId\":\"dQw4w9WgXcQ\",\"note\":\"  \"}]}" :: Maybe Mixtape
          noTracks = decode "{\"title\":\"Tape\",\"tracks\":[]}" :: Maybe Mixtape
      blankTitle `shouldBe` Nothing
      blankNote `shouldBe` Nothing
      noTracks `shouldBe` Nothing

    it "enforces field length limits and normalizes optional whitespace in mkTrack and mkMixtape" $ do
      let vid = requiredEither (mkYouTubeVideoId "dQw4w9WgXcQ")
          validTrack = requiredEither (mkTrack vid (Just "  ") (Just "  Artist  ") "  Note  ")
      validTrack.title `shouldBe` Nothing
      validTrack.artist `shouldBe` Just "Artist"
      validTrack.note `shouldBe` "Note"
      mkTrack vid (Just (T.replicate 301 "a")) Nothing "Note" `shouldSatisfy` either (const True) (const False)
      mkTrack vid Nothing (Just (T.replicate 201 "a")) "Note" `shouldSatisfy` either (const True) (const False)
      mkTrack vid Nothing Nothing (T.replicate 2001 "a") `shouldSatisfy` either (const True) (const False)

      let validTape = requiredEither (mkMixtape "  My Tape  " (Just "   ") [validTrack])
      validTape.title `shouldBe` "My Tape"
      validTape.description `shouldBe` Nothing
      mkMixtape (T.replicate 121 "a") Nothing [validTrack] `shouldSatisfy` either (const True) (const False)
      mkMixtape "Tape" (Just (T.replicate 501 "a")) [validTrack] `shouldSatisfy` either (const True) (const False)
      mkMixtape "Tape" Nothing ( replicate 101 validTrack ) `shouldSatisfy` either (const True) (const False)

    it "uses the title looked up from YouTube as the track title" $ do
      let form = CreateMixtapeForm "Tape" Nothing
            ["https://www.youtube.com/watch?v=dQw4w9WgXcQ"]
            ["Never Gonna Give You Up"]
            ["A note"]
      case createMixtapeFromForm form of
        Left err -> expectationFailure (T.unpack err)
        Right mixtape -> case mixtape.tracks of
          [Track _ (Just trackTitle) _ _] -> trackTitle `shouldBe` "Never Gonna Give You Up"
          _ -> expectationFailure "Expected the YouTube title to be the track title"

    it "keeps titles aligned with repeated video and note form fields" $ do
      let payload = BL8.pack "title=Tape&videoUrls=https%3A%2F%2Fyoutu.be%2FdQw4w9WgXcQ&videoUrls=https%3A%2F%2Fyoutu.be%2FM7lc1UVf-VE&titles=First+title&titles=Second+title&notes=First+note&notes=Second+note"
      case (urlDecodeAsForm payload :: Either Text CreateMixtapeForm) >>= createMixtapeFromForm of
        Left err -> expectationFailure (T.unpack err)
        Right mixtape -> map (\(Track _ trackTitle _ _) -> trackTitle) mixtape.tracks `shouldBe` [Just "First title", Just "Second title"]

  it "handles POST /api/mixtapes without PostgreSQL" $ do
    let video = requiredEither (mkYouTubeVideoId "dQw4w9WgXcQ")
        mixtape = Mixtape "Tape" Nothing [Track video Nothing Nothing "A note"]
        created = StoredMixtape
          { mixtapeId = requiredMaybe (mkMixtapeId "00000000-0000-4000-8000-000000000001")
          , title = "Tape"
          , description = Nothing
          , createdAt = UTCTime (fromGregorian 2026 9 27) 0
          , tracks = [StoredTrack
              { trackId = requiredMaybe (mkTrackId "00000000-0000-4000-8000-000000000002")
              , videoId = video
              , title = Nothing
              , artist = Nothing
              , note = "A note"
              }]
          }
        missingId = requiredMaybe (mkMixtapeId "00000000-0000-4000-8000-000000000099")
        app = appForStore MixtapeStore
          { storeCreateMixtape = \_ -> pure (Right created)
          , storeGetMixtape = \requestedId -> pure $ if requestedId == created.mixtapeId
              then Right (Just created)
              else Right Nothing
          }
    testWithApplication (pure app) $ \port -> do
      manager <- newManager defaultManagerSettings
      let env = mkClientEnv manager (BaseUrl Http "localhost" port "")
      landingRequest <- parseRequest ("http://localhost:" <> show port <> "/")
      landingResponse <- httpLbs landingRequest manager
      statusCode (responseStatus landingResponse) `shouldBe` 200
      let landingHeaders = responseHeaders landingResponse
      lookup "Content-Security-Policy" landingHeaders `shouldSatisfy` maybe False (const True)
      lookup "X-Content-Type-Options" landingHeaders `shouldBe` Just "nosniff"
      lookup "Referrer-Policy" landingHeaders `shouldBe` Just "strict-origin-when-cross-origin"
      lookup "X-Frame-Options" landingHeaders `shouldBe` Just "SAMEORIGIN"
      BL8.unpack (responseBody landingResponse) `shouldContain` "Make a mixtape that says a little more"
      BL8.unpack (responseBody landingResponse) `shouldContain` "/assets/favicon.svg"
      oversizedRequestBase <- parseRequest ("http://localhost:" <> show port <> "/create")
      let oversizedRequest = oversizedRequestBase
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBody = RequestBodyLBS (BL8.replicate (1024 * 1024 + 1) 'x')
            }
      oversizedResponse <- httpLbs oversizedRequest manager
      statusCode (responseStatus oversizedResponse) `shouldBe` 413
      let oversizedHeaders = responseHeaders oversizedResponse
      lookup "Content-Security-Policy" oversizedHeaders `shouldSatisfy` maybe False (const True)
      lookup "X-Content-Type-Options" oversizedHeaders `shouldSatisfy` maybe False (const True)
      lookup "Referrer-Policy" oversizedHeaders `shouldSatisfy` maybe False (const True)
      createRequest <- parseRequest ("http://localhost:" <> show port <> "/create")
      createResponse <- httpLbs createRequest manager
      statusCode (responseStatus createResponse) `shouldBe` 200
      BL8.unpack (responseBody createResponse) `shouldContain` "YouTube video link"
      BL8.unpack (responseBody createResponse) `shouldContain` "/assets/js/create.js"
      BL8.unpack (responseBody createResponse) `shouldContain` "Track title (e.g. Artist - Song, or filled from YouTube)"
      countOccurrences "data-remove" (BL8.unpack (responseBody createResponse)) `shouldBe` 2
      createScriptRequest <- parseRequest ("http://localhost:" <> show port <> "/assets/js/create.js")
      createScriptResponse <- httpLbs createScriptRequest manager
      statusCode (responseStatus createScriptResponse) `shouldBe` 200
      BL8.unpack (responseBody createScriptResponse) `shouldContain` "youtube.com/oembed"
      stylesheetRequest <- parseRequest ("http://localhost:" <> show port <> "/assets/css/app.css")
      stylesheetResponse <- httpLbs stylesheetRequest manager
      statusCode (responseStatus stylesheetResponse) `shouldBe` 200
      BL8.unpack (responseBody stylesheetResponse) `shouldContain` ".listening-layout"
      lookup "Cache-Control" (responseHeaders stylesheetResponse) `shouldBe` Just "public, max-age=86400"
      faviconRequest <- parseRequest ("http://localhost:" <> show port <> "/assets/favicon.svg")
      faviconResponse <- httpLbs faviconRequest manager
      statusCode (responseStatus faviconResponse) `shouldBe` 200
      BL8.unpack (responseBody faviconResponse) `shouldContain` "<svg"
      formRequestBase <- parseRequest ("http://localhost:" <> show port <> "/create")
      let formRequest = formRequestBase
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBody = RequestBodyLBS "title=From+the+browser&videoUrls=https%3A%2F%2Fwww.youtube.com%2Fwatch%3Fv%3DdQw4w9WgXcQ&titles=Never+Gonna+Give+You+Up&notes=Remember+this+one"
            }
      formResponse <- httpLbs formRequest manager
      statusCode (responseStatus formResponse) `shouldBe` 200
      BL8.unpack (responseBody formResponse) `shouldContain` "Your mixtape is ready"
      BL8.unpack (responseBody formResponse) `shouldContain` "/m/"
      invalidFormBase <- parseRequest ("http://localhost:" <> show port <> "/create")
      let invalidForm = invalidFormBase
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBody = RequestBodyLBS "title=Kept+form&videoUrls=https%3A%2F%2Fexample.com%2Fwatch%3Fv%3DdQw4w9WgXcQ&notes=Kept+note"
            }
      invalidFormResponse <- httpLbs invalidForm manager
      statusCode (responseStatus invalidFormResponse) `shouldBe` 422
      BL8.unpack (responseBody invalidFormResponse) `shouldContain` "Enter a YouTube video link"
      BL8.unpack (responseBody invalidFormResponse) `shouldContain` "Kept form"
      createdResult <- runClientM (postMixtape mixtape) env
      case createdResult of
        Left err -> expectationFailure (show err)
        Right response -> do
          getResponse response `shouldBe` created
          getHeaders response `shouldBe` [
            ("Location", TE.encodeUtf8 ("/m/" <> toUrlPiece (shareCodeFor created.mixtapeId)))]
      getResult <- runClientM (getMixtape created.mixtapeId) env
      getResult `shouldBe` Right created
      missingResult <- runClientM (getMixtape missingId) env
      missingResult `shouldSatisfy` either (const True) (const False)
      pageRequest <- parseRequest
        ("http://localhost:" <> show port <> "/m/" <> T.unpack (toUrlPiece (shareCodeFor created.mixtapeId)))
      pageResponse <- httpLbs pageRequest manager
      statusCode (responseStatus pageResponse) `shouldBe` 200
      BL8.unpack (responseBody pageResponse) `shouldContain` "A note"
      BL8.unpack (responseBody pageResponse) `shouldContain` "/assets/js/playback.js"
      BL8.unpack (responseBody pageResponse) `shouldContain` "Start listening"
      BL8.unpack (responseBody pageResponse) `shouldContain` "data-video-id=\"dQw4w9WgXcQ\""
      BL8.unpack (responseBody pageResponse) `shouldContain` "listening-shell"
      BL8.unpack (responseBody pageResponse) `shouldContain` "og:title"
      BL8.unpack (responseBody pageResponse) `shouldContain` "og:image"
      playbackScriptRequest <- parseRequest ("http://localhost:" <> show port <> "/assets/js/playback.js")
      playbackScriptResponse <- httpLbs playbackScriptRequest manager
      statusCode (responseStatus playbackScriptResponse) `shouldBe` 200
      BL8.unpack (responseBody playbackScriptResponse) `shouldContain` "youtube.com/iframe_api"
      missingPageRequest <- parseRequest
        ("http://localhost:" <> show port <> "/m/" <> T.unpack (toUrlPiece (shareCodeFor missingId)))
      missingPageResponse <- httpLbs missingPageRequest manager
      statusCode (responseStatus missingPageResponse) `shouldBe` 404
      lookup "Content-Type" (responseHeaders missingPageResponse) `shouldBe` Just "text/html; charset=utf-8"
      BL8.unpack (responseBody missingPageResponse) `shouldContain` "Mixtape not found"

  it "returns HTML error pages for browser routes and JSON errors for API routes when the store fails" $ do
    let sampleId = requiredMaybe (mkMixtapeId "00000000-0000-4000-8000-000000000001")
        sampleShareCode = T.unpack (toUrlPiece (shareCodeFor sampleId))
        unavailableApp = appForStore MixtapeStore
          { storeCreateMixtape = \_ -> pure (Left StoreUnavailable)
          , storeGetMixtape = \_ -> pure (Left StoreUnavailable)
          }
        failureApp = appForStore MixtapeStore
          { storeCreateMixtape = \_ -> pure (Left StoreFailure)
          , storeGetMixtape = \_ -> pure (Left StoreFailure)
          }

    testWithApplication (pure unavailableApp) $ \port -> do
      manager <- newManager defaultManagerSettings
      pageReq <- parseRequest ("http://localhost:" <> show port <> "/m/" <> sampleShareCode)
      pageResp <- httpLbs pageReq manager
      statusCode (responseStatus pageResp) `shouldBe` 503
      lookup "Content-Type" (responseHeaders pageResp) `shouldBe` Just "text/html; charset=utf-8"
      BL8.unpack (responseBody pageResp) `shouldContain` "Temporarily unavailable"

      createdReq <- parseRequest ("http://localhost:" <> show port <> "/created/" <> sampleShareCode)
      createdResp <- httpLbs createdReq manager
      statusCode (responseStatus createdResp) `shouldBe` 503
      lookup "Content-Type" (responseHeaders createdResp) `shouldBe` Just "text/html; charset=utf-8"

      formReqBase <- parseRequest ("http://localhost:" <> show port <> "/create")
      let formReq = formReqBase
            { method = "POST"
            , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
            , requestBody = RequestBodyLBS "title=My+Tape&videoUrls=https%3A%2F%2Fyoutu.be%2FdQw4w9WgXcQ&notes=Note"
            }
      formResp <- httpLbs formReq manager
      statusCode (responseStatus formResp) `shouldBe` 503
      lookup "Content-Type" (responseHeaders formResp) `shouldBe` Just "text/html; charset=utf-8"
      BL8.unpack (responseBody formResp) `shouldContain` "We could not save your mixtape"

      apiReq <- parseRequest ("http://localhost:" <> show port <> "/api/mixtapes/" <> T.unpack (toUrlPiece sampleId))
      apiResp <- httpLbs apiReq manager
      statusCode (responseStatus apiResp) `shouldBe` 503
      lookup "Content-Type" (responseHeaders apiResp) `shouldBe` Just "application/json"
      BL8.unpack (responseBody apiResp) `shouldContain` "storage_unavailable"

    testWithApplication (pure failureApp) $ \port -> do
      manager <- newManager defaultManagerSettings
      pageReq <- parseRequest ("http://localhost:" <> show port <> "/m/" <> sampleShareCode)
      pageResp <- httpLbs pageReq manager
      statusCode (responseStatus pageResp) `shouldBe` 500
      lookup "Content-Type" (responseHeaders pageResp) `shouldBe` Just "text/html; charset=utf-8"
      BL8.unpack (responseBody pageResp) `shouldContain` "Something went wrong"

      apiReq <- parseRequest ("http://localhost:" <> show port <> "/api/mixtapes/" <> T.unpack (toUrlPiece sampleId))
      apiResp <- httpLbs apiReq manager
      statusCode (responseStatus apiResp) `shouldBe` 500
      lookup "Content-Type" (responseHeaders apiResp) `shouldBe` Just "application/json"
      BL8.unpack (responseBody apiResp) `shouldContain` "internal_error"

requiredMaybe :: Maybe a -> a
requiredMaybe = maybe (error "invalid UUID fixture") id

requiredEither :: Either e a -> a
requiredEither = either (const (error "invalid video ID fixture")) id

countOccurrences :: String -> String -> Int
countOccurrences needle = length . filter (isPrefixOf needle) . tails
