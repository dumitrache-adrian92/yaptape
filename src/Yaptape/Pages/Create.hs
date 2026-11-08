{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Pages.Create
  ( renderCreatePage
  , renderCreatedPage
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (zip4)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Lucid
import Lucid.Base (makeAttribute)
import Servant.API (ToHttpApiData (toUrlPiece))
import Yaptape.Domain
  ( StoredMixtape (..)
  , maxMixtapeDescriptionLength
  , maxMixtapeTitleLength
  , maxTrackArtistLength
  , maxTrackNoteLength
  , maxTrackTitleLength
  , shareCodeFor
  )
import Yaptape.Form (CreateMixtapeForm (..))
import Yaptape.Pages.Layout (layout, versionedAsset)
import Yaptape.YouTube (youTubeUrlSpec)

renderCreatePage :: Maybe Text -> Maybe CreateMixtapeForm -> Html ()
renderCreatePage errorMessage previousForm = layout "Make a mixtape · Yaptape" $ main_ [class_ "page-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  section_ [id_ "create-flow", class_ "create-card", makeAttribute "data-youtube-spec" serializedYouTubeSpec] $ do
    p_ [class_ "eyebrow"] "MAKE YOUR TAPE"
    h1_ "Pick the songs. Add the words."
    p_ [class_ "muted"] "Paste a YouTube link for each track and leave a note for the person listening."
    maybe mempty (p_ [class_ "error", role_ "alert"] . toHtml) errorMessage
    form_ [action_ "/create", method_ "post", makeAttribute "hx-post" "/create", makeAttribute "hx-target" "#create-flow", makeAttribute "hx-select" "#create-flow", makeAttribute "hx-swap" "outerHTML"] $ do
      label_ [class_ "field-label", for_ "tape-title"] "Mixtape title"
      input_ ([id_ "tape-title", name_ "title", type_ "text", placeholder_ "Songs for the long way home", required_ "", maxlength_ (T.pack (show maxMixtapeTitleLength))] <> maybe [] (\form -> [value_ form.formTitle]) previousForm)
      label_ [class_ "field-label", for_ "tape-description"] "A few words about this tape (optional)"
      textarea_ [id_ "tape-description", name_ "description", rows_ "2", placeholder_ "Set the scene…", maxlength_ (T.pack (show maxMixtapeDescriptionLength))] (maybe mempty (toHtml . fromMaybe "" . formDescription) previousForm)
      div_ [class_ "tracks-heading"] $ do
        h2_ "Tracklist"
        button_ [type_ "button", id_ "add-track", class_ "quiet-button"] "+ Add a track"
      div_ [id_ "track-list"] (mapM_ trackRow (formRows previousForm))
      template_ [id_ "track-template"] (trackRow Nothing)
      button_ [type_ "submit", class_ "button submit-button"] "Create mixtape"
      p_ [class_ "fine-print"] "You can rearrange tracks before sharing your tape."
  script_ [src_ "/assets/js/htmx.min.js", defer_ "", makeAttribute "integrity" "sha256-Iig+9oy3VFkU8KiKG97cclanA9HVgMHSVSF9ClDTExM=", makeAttribute "crossorigin" "anonymous"] (mempty :: Html ())
  script_ [src_ (versionedAsset "/assets/js/create.js"), defer_ ""] (mempty :: Html ())
  where
    serializedYouTubeSpec = TE.decodeUtf8 (LBS.toStrict (encode youTubeUrlSpec))

formRows :: Maybe CreateMixtapeForm -> [Maybe (Text, Text, Text, Text)]
formRows Nothing = [Nothing]
formRows (Just form) = case zip4 form.formVideoUrls form.formNotes (form.formTitles <> repeat "") (form.formArtists <> repeat "") of
  [] -> [Nothing]
  entries -> map Just entries

trackRow :: Maybe (Text, Text, Text, Text) -> Html ()
trackRow savedValues = div_ [class_ "track-row"] $ do
  div_ [class_ "track-number"] "♪"
  div_ [class_ "track-fields"] $ do
    input_ ([name_ "videoUrls", type_ "url", placeholder_ "YouTube video link", required_ "", class_ "video-url"] <> maybe [] (\(url, _, _, _) -> [value_ url]) savedValues)
    input_ ([name_ "titles", type_ "text", placeholder_ "Track title (e.g. Artist - Song, or filled from YouTube)", class_ "track-title", maxlength_ (T.pack (show maxTrackTitleLength))] <> maybe [] (\(_, _, trackTitle, _) -> [value_ trackTitle]) savedValues)
    input_ ([name_ "artists", type_ "text", placeholder_ "Artist (optional)", class_ "track-artist", maxlength_ (T.pack (show maxTrackArtistLength))] <> maybe [] (\(_, _, _, trackArtist) -> [value_ trackArtist]) savedValues)
    textarea_ [name_ "notes", rows_ "2", placeholder_ "What should they notice while this plays?", required_ "", class_ "track-note", maxlength_ (T.pack (show maxTrackNoteLength))] (maybe mempty (toHtml . (\(_, trackNote, _, _) -> trackNote)) savedValues)
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
    script_ [src_ (versionedAsset "/assets/js/copy-link.js"), defer_ ""] (mempty :: Html ())
