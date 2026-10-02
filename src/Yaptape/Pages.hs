{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Pages
  ( renderLandingPage
  , renderCreatePage
  , renderCreatedPage
  , renderMixtapePage
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
import Lucid.Base (makeAttribute)
import Yaptape.Domain (StoredMixtape (..), StoredTrack (..), shareCodeFor)
import Yaptape.Form (CreateMixtapeForm (..))
import Servant.API (ToHttpApiData (toUrlPiece))
import Yaptape.YouTube (unYouTubeVideoId)

renderLandingPage :: Html ()
renderLandingPage = layout "Yaptape" $ main_ [class_ "landing"] $ do
  p_ [class_ "eyebrow"] "SIDE A · A LITTLE MORE TO THE MUSIC"
  h1_ "Make a mixtape that says a little more."
  p_ [class_ "intro"] "Bring together the songs you love, add a note to each one, and send someone a tape they can listen through."
  a_ [href_ "/create", class_ "button"] "Make a mixtape"
  div_ [class_ "tape-art", makeAttribute "aria-hidden" "true"] $ do
    div_ [class_ "reel reel-one"] mempty
    div_ [class_ "reel reel-two"] mempty

renderCreatePage :: Maybe Text -> Maybe CreateMixtapeForm -> Html ()
renderCreatePage errorMessage previousForm = layout "Make a mixtape · Yaptape" $ main_ [class_ "page-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  section_ [id_ "create-flow", class_ "create-card"] $ do
    p_ [class_ "eyebrow"] "MAKE YOUR TAPE"
    h1_ "Pick the songs. Add the words."
    p_ [class_ "muted"] "Paste a YouTube link for each track and leave a note for the person listening."
    maybe mempty (p_ [class_ "error", role_ "alert"] . toHtml) errorMessage
    form_ [action_ "/create", method_ "post", makeAttribute "hx-post" "/create", makeAttribute "hx-target" "#create-flow", makeAttribute "hx-select" "#create-flow", makeAttribute "hx-swap" "outerHTML"] $ do
      label_ [class_ "field-label", for_ "tape-title"] "Mixtape title"
      input_ ([id_ "tape-title", name_ "title", type_ "text", placeholder_ "Songs for the long way home", required_ "", maxlength_ "120"] <> maybe [] (\form -> [value_ form.formTitle]) previousForm)
      label_ [class_ "field-label", for_ "tape-description"] "A few words about this tape (optional)"
      textarea_ [id_ "tape-description", name_ "description", rows_ "2", placeholder_ "Set the scene…", maxlength_ "500"] (maybe mempty (toHtml . fromMaybe "" . formDescription) previousForm)
      div_ [class_ "tracks-heading"] $ do
        h2_ "Tracklist"
        button_ [type_ "button", id_ "add-track", class_ "quiet-button"] "+ Add a track"
      div_ [id_ "track-list"] (mapM_ trackRow (formRows previousForm))
      template_ [id_ "track-template"] (trackRow Nothing)
      button_ [type_ "submit", class_ "button submit-button"] "Create mixtape"
      p_ [class_ "fine-print"] "You can rearrange tracks before sharing your tape."
  script_ [src_ "/assets/js/htmx.min.js", defer_ "", makeAttribute "integrity" "sha256-Iig+9oy3VFkU8KiKG97cclanA9HVgMHSVSF9ClDTExM=", makeAttribute "crossorigin" "anonymous"] (mempty :: Html ())
  script_ [src_ "/assets/js/create.js", defer_ ""] (mempty :: Html ())

formRows :: Maybe CreateMixtapeForm -> [Maybe (Text, Text, Text)]
formRows Nothing = [Nothing]
formRows (Just form) = case zip3 form.formVideoUrls form.formNotes (form.formTitles <> repeat "") of
  [] -> [Nothing]
  entries -> map Just entries

trackRow :: Maybe (Text, Text, Text) -> Html ()
trackRow savedValues = div_ [class_ "track-row"] $ do
  div_ [class_ "track-number"] "♪"
  div_ [class_ "track-fields"] $ do
    input_ ([name_ "videoUrls", type_ "url", placeholder_ "YouTube video link", required_ "", class_ "video-url"] <> maybe [] (\(url, _, _) -> [value_ url]) savedValues)
    input_ ([name_ "titles", type_ "text", placeholder_ "Track title (e.g. Artist - Song, or filled from YouTube)", class_ "track-title", maxlength_ "300"] <> maybe [] (\(_, _, trackTitle) -> [value_ trackTitle]) savedValues)
    textarea_ [name_ "notes", rows_ "2", placeholder_ "What should they notice while this plays?", required_ "", class_ "track-note"] (maybe mempty (toHtml . (\(_, trackNote, _) -> trackNote)) savedValues)
    p_ [class_ "url-hint", makeAttribute "aria-live" "polite"] "YouTube links from youtube.com or youtu.be"
  div_ [class_ "row-actions"] $ do
    button_ [type_ "button", makeAttribute "data-move" "up", class_ "icon-button", title_ "Move track up"] "↑"
    button_ [type_ "button", makeAttribute "data-move" "down", class_ "icon-button", title_ "Move track down"] "↓"
    button_ [type_ "button", makeAttribute "data-remove" "", class_ "icon-button remove-button", title_ "Remove track"] "×"

renderCreatedPage :: StoredMixtape -> Html ()
renderCreatedPage mixtape = layout "Your mixtape is ready · Yaptape" $ main_ [class_ "page-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  section_ [id_ "create-flow", class_ "create-card success-card"] $ do
    p_ [class_ "eyebrow"] "READY TO SEND"
    h1_ "Your mixtape is ready."
    p_ [class_ "muted"] "Send this link to someone you want to share it with."
    div_ [class_ "share-box"] $ do
      input_ [id_ "share-link", readonly_ "", value_ ("/m/" <> toUrlPiece (shareCodeFor mixtape.mixtapeId))]
      button_ [type_ "button", class_ "button", makeAttribute "data-copy" "#share-link"] "Copy link"
    p_ [id_ "copy-status", class_ "fine-print", makeAttribute "aria-live" "polite"] mempty
    a_ [href_ ("/m/" <> toUrlPiece (shareCodeFor mixtape.mixtapeId)), class_ "text-link"] "Preview your mixtape →"
    a_ [href_ "/create", class_ "text-link secondary-link"] "Make another"
    script_ [src_ "/assets/js/copy-link.js", defer_ ""] (mempty :: Html ())

data PageMeta = PageMeta
  { metaTitle :: Text
  , metaDescription :: Maybe Text
  , metaImage :: Maybe Text
  , metaOgType :: Maybe Text
  }

defaultMeta :: Text -> PageMeta
defaultMeta title = PageMeta
  { metaTitle = title
  , metaDescription = Just "Make a mixtape from YouTube tracks, add a note to each track, and share it."
  , metaImage = Nothing
  , metaOgType = Just "website"
  }

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
  script_ [src_ "/assets/js/playback.js", defer_ ""] (mempty :: Html ())
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

layout :: Text -> Html () -> Html ()
layout pageTitle = layoutWithMeta (defaultMeta pageTitle)

layoutWithMeta :: PageMeta -> Html () -> Html ()
layoutWithMeta pageMeta content = doctypehtml_ $ html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml pageMeta.metaTitle)
    maybe mempty (\desc -> meta_ [name_ "description", content_ desc]) pageMeta.metaDescription
    meta_ [makeAttribute "property" "og:title", content_ pageMeta.metaTitle]
    maybe mempty (\desc -> meta_ [makeAttribute "property" "og:description", content_ desc]) pageMeta.metaDescription
    maybe mempty (\ogType -> meta_ [makeAttribute "property" "og:type", content_ ogType]) pageMeta.metaOgType
    maybe mempty (\img -> do
      meta_ [makeAttribute "property" "og:image", content_ img]
      meta_ [name_ "twitter:image", content_ img]
      ) pageMeta.metaImage
    meta_ [name_ "twitter:card", content_ (if maybe False (const True) pageMeta.metaImage then "summary_large_image" else "summary")]
    meta_ [name_ "twitter:title", content_ pageMeta.metaTitle]
    maybe mempty (\desc -> meta_ [name_ "twitter:description", content_ desc]) pageMeta.metaDescription
    link_ [rel_ "icon", type_ "image/svg+xml", href_ "/assets/favicon.svg"]
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ "anonymous"]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Playfair+Display:wght@500;600;700&display=swap"]
    link_ [rel_ "stylesheet", href_ "/assets/css/app.css"]
  body_ content

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
