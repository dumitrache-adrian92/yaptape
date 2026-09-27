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
import Lucid
import Lucid.Base (makeAttribute, makeElement)
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
      div_ [id_ "track-list"] (mapM_ (trackRow True) (formRows previousForm))
      template_ [id_ "track-template"] (trackRow False Nothing)
      button_ [type_ "submit", class_ "button submit-button"] "Create mixtape"
      p_ [class_ "fine-print"] "You can rearrange tracks before sharing your tape."
  script_ [src_ "https://unpkg.com/htmx.org@2.0.8", defer_ ""] (mempty :: Html ())
  script_ [type_ "text/javascript"] (toHtmlRaw trackEditorScript)

formRows :: Maybe CreateMixtapeForm -> [Maybe (Text, Text)]
formRows Nothing = [Nothing]
formRows (Just form) = case zip form.formVideoUrls form.formNotes of
  [] -> [Nothing]
  entries -> map Just entries

trackRow :: Bool -> Maybe (Text, Text) -> Html ()
trackRow removable savedValues = div_ [class_ "track-row"] $ do
  div_ [class_ "track-number"] "♪"
  div_ [class_ "track-fields"] $ do
    input_ ([name_ "videoUrls", type_ "url", placeholder_ "YouTube video link", required_ "", class_ "video-url"] <> maybe [] (\(url, _) -> [value_ url]) savedValues)
    textarea_ [name_ "notes", rows_ "2", placeholder_ "What should they notice while this plays?", required_ "", class_ "track-note"] (maybe mempty (toHtml . snd) savedValues)
    p_ [class_ "url-hint", makeAttribute "aria-live" "polite"] "YouTube links from youtube.com or youtu.be"
  div_ [class_ "row-actions"] $ do
    button_ [type_ "button", makeAttribute "data-move" "up", class_ "icon-button", title_ "Move track up"] "↑"
    button_ [type_ "button", makeAttribute "data-move" "down", class_ "icon-button", title_ "Move track down"] "↓"
    if removable then button_ [type_ "button", makeAttribute "data-remove" "", class_ "icon-button remove-button", title_ "Remove track"] "×" else mempty

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
    script_ [type_ "text/javascript"] (toHtmlRaw copyScript)

renderMixtapePage :: StoredMixtape -> Html ()
renderMixtapePage mixtape = layout (mixtape.title <> " · Yaptape") $ main_ [class_ "page-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  article_ [class_ "create-card"] $ do
    p_ [class_ "eyebrow"] "A MIXTAPE FOR YOU"
    h1_ (toHtml mixtape.title)
    maybe mempty (p_ [class_ "muted"] . toHtml) mixtape.description
    ol_ [class_ "shared-tracklist"] $ mapM_ renderTrack mixtape.tracks

renderTrack :: StoredTrack -> Html ()
renderTrack track = li_ [class_ "shared-track"] $ do
  a_ [href_ ("https://www.youtube.com/watch?v=" <> unYouTubeVideoId track.videoId), class_ "text-link"]
    (toHtml (fromMaybe (unYouTubeVideoId track.videoId) track.title))
  maybe mempty (p_ [class_ "muted"] . toHtml) track.artist
  p_ [class_ "track-note-display"] (toHtml track.note)

layout :: Text -> Html () -> Html ()
layout pageTitle content = doctypehtml_ $ html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml pageTitle)
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ "anonymous"]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Playfair+Display:wght@500;600;700&display=swap"]
    makeElement "style" (toHtmlRaw stylesheet)
  body_ content

stylesheet :: Text
stylesheet = ""
  <> "*{box-sizing:border-box}body{margin:0;background:#f4efe5;color:#222019;font-family:'DM Sans',sans-serif}"
  <> ".page-shell,.landing{width:min(100% - 36px,760px);margin:0 auto;padding:34px 0 80px}.landing{min-height:100vh;display:flex;flex-direction:column;justify-content:center;position:relative}"
  <> ".wordmark{display:inline-block;margin-bottom:48px;color:#30271f;text-decoration:none;font-weight:700;letter-spacing:.08em;text-transform:uppercase}.eyebrow{font-size:.72rem;font-weight:700;letter-spacing:.17em;color:#a14f38}"
  <> "h1{font:600 clamp(2.5rem,7vw,4.7rem)/1.04 'Playfair Display',serif;letter-spacing:-.045em;margin:12px 0 18px;max-width:700px}h2{font:600 1.4rem 'Playfair Display',serif;margin:0}"
  <> ".intro{font-size:1.15rem;line-height:1.7;max-width:540px;color:#625c51;margin:0 0 30px}.muted{color:#716b61;line-height:1.65}.create-card{background:#fffdf8;border:1px solid #e5ddcf;border-radius:22px;padding:clamp(24px,5vw,48px);box-shadow:0 18px 50px #3127190d}.field-label{display:block;font-size:.88rem;font-weight:600;margin:22px 0 8px}"
  <> "input,textarea{width:100%;font:inherit;color:#222019;background:#fff;border:1px solid #dcd4c6;border-radius:10px;padding:13px 14px;outline:none}input:focus,textarea:focus{border-color:#a14f38;box-shadow:0 0 0 3px #a14f381c}textarea{resize:vertical}"
  <> ".button{display:inline-flex;align-items:center;justify-content:center;background:#a14f38;border:0;border-radius:999px;color:white;text-decoration:none;font:600 .95rem 'DM Sans',sans-serif;padding:14px 22px;cursor:pointer}.button:hover{background:#843d2b}.tracks-heading{display:flex;align-items:center;justify-content:space-between;margin:32px 0 12px}.quiet-button,.icon-button{border:0;background:transparent;color:#a14f38;font:600 .9rem 'DM Sans',sans-serif;cursor:pointer}.track-row{display:grid;grid-template-columns:34px 1fr auto;gap:12px;padding:16px 0;border-top:1px solid #eee7db}.track-number{color:#a14f38;padding-top:12px}.track-fields{min-width:0}.track-fields textarea{margin-top:9px}.url-hint,.fine-print{font-size:.78rem;color:#827a6e;margin:7px 0}.url-valid{color:#47734d}.url-invalid{color:#a14f38}.row-actions{display:flex;flex-direction:column;gap:2px}.icon-button{font-size:1.1rem;color:#766c5d;padding:6px}.remove-button{color:#a14f38}.submit-button{margin-top:18px}.error{padding:12px 14px;border-radius:9px;background:#fbebe6;color:#923c2b}.share-box{display:flex;gap:10px;margin:28px 0 12px}.share-box input{flex:1}.text-link{display:inline-block;color:#a14f38;font-weight:600;text-decoration:none;margin:14px 18px 0 0}.secondary-link{color:#716b61}.success-card h1{font-size:clamp(2.5rem,6vw,3.8rem)}.tape-art{position:absolute;right:2%;bottom:9%;width:210px;height:132px;background:#cb6548;border:7px solid #24211d;border-radius:12px;transform:rotate(-8deg);box-shadow:14px 18px 0 #dbd0be}.reel{position:absolute;top:27px;width:56px;height:56px;border:8px solid #29251f;border-radius:50%;background:repeating-conic-gradient(#29251f 0 12deg,#f0d6ad 12deg 25deg)}.reel-one{left:27px}.reel-two{right:27px}.shared-tracklist{padding-left:26px}.shared-track{padding:16px 10px;border-top:1px solid #eee7db}.track-note-display{background:#f7f1e7;border-radius:10px;padding:14px;line-height:1.6}@media(max-width:600px){.tape-art{position:relative;right:auto;bottom:auto;margin:50px auto 0;transform:rotate(-6deg)}.share-box{flex-direction:column}.row-actions{gap:0}.track-row{grid-template-columns:25px 1fr auto;gap:6px}}"

trackEditorScript :: Text
trackEditorScript = "document.addEventListener('input',function(e){if(!e.target.matches('.video-url'))return;const hint=e.target.parentElement.querySelector('.url-hint');try{const u=new URL(e.target.value),host=u.hostname.toLowerCase(),parts=u.pathname.split('/').filter(Boolean);let id='';if(['youtube.com','www.youtube.com','m.youtube.com','music.youtube.com'].includes(host)){if(parts[0]==='watch')id=u.searchParams.get('v')||'';else if(['shorts','embed','live'].includes(parts[0]))id=parts[1]||''}else if(['youtu.be','www.youtu.be'].includes(host))id=parts[0]||'';const valid=['https:','http:'].includes(u.protocol)&&/^[A-Za-z0-9_-]{11}$/.test(id);hint.textContent=valid?'YouTube link looks good.':'Use a youtube.com or youtu.be video link.';hint.classList.toggle('url-valid',valid);hint.classList.toggle('url-invalid',!valid)}catch(_){hint.textContent='Paste a YouTube video link.';hint.classList.remove('url-valid');hint.classList.add('url-invalid')}});document.addEventListener('click',function(e){const add=e.target.closest('#add-track');if(add){const list=document.querySelector('#track-list'),row=document.querySelector('#track-template').content.firstElementChild.cloneNode(true);list.append(row);row.querySelector('input').focus();return}const move=e.target.closest('[data-move]');if(move){const row=move.closest('.track-row');if(move.dataset.move==='up'&&row.previousElementSibling)row.parentNode.insertBefore(row,row.previousElementSibling);if(move.dataset.move==='down'&&row.nextElementSibling)row.parentNode.insertBefore(row.nextElementSibling,row);return}const remove=e.target.closest('[data-remove]');if(remove){const rows=document.querySelectorAll('#track-list .track-row');if(rows.length>1)remove.closest('.track-row').remove()}});document.body.addEventListener('htmx:responseError',function(){document.querySelector('#create-flow').insertAdjacentHTML('afterbegin','<p class=error role=alert>We could not save this mixtape. Please try again.</p>')});"

copyScript :: Text
copyScript = "const input=document.querySelector('#share-link');input.value=new URL(input.value,location.origin).href;document.addEventListener('click',async function(e){const button=e.target.closest('[data-copy]');if(!button)return;try{await navigator.clipboard.writeText(input.value);document.querySelector('#copy-status').textContent='Link copied to clipboard.'}catch(_){input.select();document.execCommand('copy');document.querySelector('#copy-status').textContent='Link copied.'}});"
