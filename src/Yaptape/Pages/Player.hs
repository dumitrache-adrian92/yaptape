{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Pages.Player
  ( renderMixtapePage
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)
import Yaptape.Domain
  ( StoredMixtape (..)
  , StoredTrack (..)
  )
import Yaptape.Pages.Layout (PageMeta (..), layoutWithMeta, versionedAsset)
import Yaptape.YouTube (unYouTubeVideoId)

renderMixtapePage :: StoredMixtape -> Html ()
renderMixtapePage mixtape = layoutWithMeta tapeMeta $ div_ [class_ "page-shell listening-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  main_ [class_ "listening-page", id_ "mixtape-player"] $ do
    header_ [class_ "listening-heading"] $ do
      p_ [class_ "eyebrow"] "A MIXTAPE FOR YOU"
      h1_ (toHtml mixtape.title)
      maybe mempty (p_ [class_ "muted"] . toHtml) mixtape.description
    div_ [class_ "listening-layout"] $ do
      section_ [class_ "player-column", makeAttribute "aria-label" "Mixtape player"] $ do
        div_ [class_ "player-deck", id_ "player-deck"] $ do
          div_ [class_ "cassette", id_ "cassette"] $ do
            div_ [class_ "cassette-label"] $ do
              p_ [class_ "eyebrow"] "YAPTAPE · SIDE A"
              p_ [class_ "cassette-title"] (toHtml mixtape.title)
            div_ [class_ "cassette-reel reel-left"] (div_ [class_ "reel-hub"] mempty)
            div_ [class_ "cassette-reel reel-right"] (div_ [class_ "reel-hub"] mempty)
            div_ [class_ "cassette-window"] mempty
          div_ [class_ "deck-slot"] mempty
        div_ [class_ "video-frame"] (div_ [id_ "youtube-player"] mempty)
        div_ [class_ "transport"] $ do
          button_ [type_ "button", id_ "previous-track", class_ "transport-button", makeAttribute "aria-label" "Play previous track"] "Previous"
          button_ [type_ "button", id_ "toggle-playback", class_ "button play-button", makeAttribute "aria-pressed" "false"] "Play"
          button_ [type_ "button", id_ "next-track", class_ "transport-button", makeAttribute "aria-label" "Play next track"] "Next"
        p_ [id_ "playback-status", class_ "playback-status", role_ "status", makeAttribute "aria-live" "polite"] "Loading the first track…"
        button_ [type_ "button", id_ "start-playback", class_ "button start-button", hidden_ ""] "Start listening"
      aside_ [class_ "note-panel"] $ do
        p_ [class_ "eyebrow"] "A NOTE FOR THIS TRACK"
        p_ [id_ "current-track-title", class_ "current-track-title"] mempty
        p_ [id_ "current-track-artist", class_ "current-track-artist"] mempty
        blockquote_ [id_ "current-track-note", class_ "current-track-note"] mempty
        p_ [class_ "track-count"] (toHtml (show (length mixtape.tracks) <> " tracks on this tape"))
    section_ [class_ "queue-section"] $ do
      div_ [class_ "queue-heading"] $ do
        h2_ "The tracklist"
        p_ [id_ "current-track-counter", class_ "track-count"] ""
      ol_ [id_ "track-queue", class_ "playback-queue"] (mapM_ (uncurry renderQueueTrack) (zip [0 :: Int ..] mixtape.tracks))
  script_ [src_ (versionedAsset "/assets/js/playback.js"), defer_ ""] (mempty :: Html ())
  where
    firstThumbnail = case mixtape.tracks of
      (t : _) -> Just ("https://i.ytimg.com/vi/" <> unYouTubeVideoId t.videoId <> "/hqdefault.jpg")
      [] -> Nothing
    tapeDesc = case mixtape.description of
      Just d | not (T.null (T.strip d)) -> Just d
      _ -> Just ("A mixtape with " <> T.pack (show (length mixtape.tracks)) <> " tracks on Yaptape.")
    tapeMeta = PageMeta
      { metaTitle = mixtape.title <> " · Yaptape"
      , metaDescription = tapeDesc
      , metaImage = firstThumbnail
      , metaOgType = Just "music.playlist"
      }

renderQueueTrack :: Int -> StoredTrack -> Html ()
renderQueueTrack index track = li_ $ button_ attributes $ do
  span_ [class_ "queue-number"] (toHtml (trackNumber <> "."))
  span_ $ do
    span_ [class_ "queue-track-title"] (toHtml (fromMaybe (unYouTubeVideoId track.videoId) track.title))
    maybe mempty (span_ [class_ "queue-track-artist"] . toHtml . (" · " <>)) track.artist
  span_ [class_ "queue-indicator"] ""
  where
    trackNumber = show (index + 1)
    attributes =
      [ type_ "button"
      , class_ "queue-track"
      , makeAttribute "data-track-index" (T.pack (show index))
      , makeAttribute "data-video-id" (unYouTubeVideoId track.videoId)
      , makeAttribute "data-track-title" (fromMaybe (unYouTubeVideoId track.videoId) track.title)
      , makeAttribute "data-track-note" track.note
      ] <> maybe [] (\artistName -> [makeAttribute "data-track-artist" artistName]) track.artist
