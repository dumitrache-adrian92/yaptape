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

formRows :: Maybe CreateMixtapeForm -> [Maybe (Text, Text, Text)]
formRows Nothing = [Nothing]
formRows (Just form) = case zip3 form.formVideoUrls form.formNotes (form.formTitles <> repeat "") of
  [] -> [Nothing]
  entries -> map Just entries

trackRow :: Bool -> Maybe (Text, Text, Text) -> Html ()
trackRow removable savedValues = div_ [class_ "track-row"] $ do
  div_ [class_ "track-number"] "♪"
  div_ [class_ "track-fields"] $ do
    input_ ([name_ "videoUrls", type_ "url", placeholder_ "YouTube video link", required_ "", class_ "video-url"] <> maybe [] (\(url, _, _) -> [value_ url]) savedValues)
    input_ ([name_ "titles", type_ "text", placeholder_ "Track title (filled from YouTube)", class_ "track-title", maxlength_ "300"] <> maybe [] (\(_, _, trackTitle) -> [value_ trackTitle]) savedValues)
    textarea_ [name_ "notes", rows_ "2", placeholder_ "What should they notice while this plays?", required_ "", class_ "track-note"] (maybe mempty (toHtml . (\(_, trackNote, _) -> trackNote)) savedValues)
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
renderMixtapePage mixtape = layout (mixtape.title <> " · Yaptape") $ div_ [class_ "page-shell listening-shell"] $ do
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
          button_ [type_ "button", id_ "toggle-playback", class_ "button play-button"] "Play"
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
  script_ [type_ "text/javascript"] (toHtmlRaw playbackScript)

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
  <> ".page-shell,.landing{width:min(100% - 36px,760px);margin:0 auto;padding:34px 0 80px}.page-shell.listening-shell{width:min(100% - 48px,1240px)}.landing{min-height:100vh;display:flex;flex-direction:column;justify-content:center;position:relative}"
  <> ".wordmark{display:inline-block;margin-bottom:48px;color:#30271f;text-decoration:none;font-weight:700;letter-spacing:.08em;text-transform:uppercase}.eyebrow{font-size:.72rem;font-weight:700;letter-spacing:.17em;color:#a14f38}"
  <> "h1{font:600 clamp(2.5rem,7vw,4.7rem)/1.04 'Playfair Display',serif;letter-spacing:-.045em;margin:12px 0 18px;max-width:700px}h2{font:600 1.4rem 'Playfair Display',serif;margin:0}"
  <> ".intro{font-size:1.15rem;line-height:1.7;max-width:540px;color:#625c51;margin:0 0 30px}.muted{color:#716b61;line-height:1.65}.create-card{background:#fffdf8;border:1px solid #e5ddcf;border-radius:22px;padding:clamp(24px,5vw,48px);box-shadow:0 18px 50px #3127190d}.field-label{display:block;font-size:.88rem;font-weight:600;margin:22px 0 8px}"
  <> ".listening-page{max-width:1040px;margin:0 auto}.listening-heading{max-width:720px;margin:14px 0 32px}.listening-heading h1{font-size:clamp(2.5rem,6vw,4rem)}.listening-layout{display:grid;grid-template-columns:minmax(0,1.15fr) minmax(260px,.85fr);gap:22px;align-items:stretch}.player-column,.note-panel{background:#fffdf8;border:1px solid #e5ddcf;border-radius:20px;padding:24px;box-shadow:0 18px 50px #3127190d}.player-deck{height:155px;background:#342e27;border:8px solid #24211d;border-radius:14px;position:relative;display:flex;align-items:flex-end;justify-content:center;overflow:hidden;margin-bottom:18px}.deck-slot{position:absolute;top:16px;left:10%;right:10%;height:9px;background:#171512;border-radius:99px;box-shadow:0 3px 0 #594f42}.cassette{position:absolute;z-index:1;top:28px;width:min(76%,360px);height:120px;background:#cb6548;border:5px solid #211e1a;border-radius:9px;display:grid;grid-template-columns:1fr 52px 52px;align-items:center;gap:12px;padding:12px 16px;box-shadow:0 6px 0 #863f2d;animation:tape-in .9s cubic-bezier(.2,.8,.2,1) both}.cassette-label{background:#f1dfbd;border-radius:4px;padding:8px 12px;min-width:0}.cassette-label .eyebrow{font-size:.55rem;margin:0}.cassette-title{font:600 .86rem 'DM Sans',sans-serif;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;margin:6px 0 0}.cassette-reel{width:44px;height:44px;border:6px solid #27231e;border-radius:50%;background:repeating-conic-gradient(#27231e 0 13deg,#e5d2af 13deg 27deg);display:grid;place-items:center}.reel-hub{width:12px;height:12px;background:#cb6548;border:3px solid #27231e;border-radius:50%}.cassette-window{position:absolute;bottom:5px;left:43%;width:14%;height:8px;border-radius:4px;background:#4a332b}.player-deck[data-playing=true] .cassette-reel{animation:reel-spin 2s linear infinite}.video-frame{aspect-ratio:16/9;background:#171512;border-radius:12px;overflow:hidden}.video-frame iframe{width:100%;height:100%;border:0}.transport{display:flex;align-items:center;justify-content:center;gap:18px;margin:18px 0 8px}.transport-button{background:transparent;border:0;color:#61584e;font:600 .9rem 'DM Sans',sans-serif;padding:12px;cursor:pointer}.transport-button:disabled{opacity:.4;cursor:default}.play-button{min-width:124px}.start-button{margin-top:10px}.start-button[hidden]{display:none}.playback-status{text-align:center;min-height:1.5em;color:#716b61;font-size:.85rem}.note-panel{display:flex;flex-direction:column;justify-content:center;min-height:270px}.note-panel .eyebrow{margin-top:0}.current-track-title{font:600 clamp(1.4rem,3vw,2rem)/1.2 'Playfair Display',serif;margin:8px 0}.current-track-artist{color:#716b61;margin:0}.current-track-note{font:500 1.15rem/1.7 'Playfair Display',serif;margin:24px 0 12px;padding:0}.track-count{font-size:.8rem;color:#827a6e}.queue-section{margin-top:34px}.queue-heading{display:flex;align-items:baseline;justify-content:space-between}.playback-queue{list-style:none;padding:0;margin:10px 0;border-top:1px solid #e5ddcf}.queue-track{display:grid;grid-template-columns:44px 1fr auto;gap:14px;align-items:center;width:100%;padding:16px 12px;border:0;border-bottom:1px solid #e5ddcf;background:transparent;color:#30271f;text-align:left;cursor:pointer}.queue-track:hover,.queue-track[aria-current=true]{background:#fffdf8}.queue-number{color:#a14f38;font-variant-numeric:tabular-nums}.queue-track-title,.queue-track-artist{display:block}.queue-track-title{font-weight:600}.queue-track-artist{font-size:.82rem;color:#827a6e}.queue-indicator{color:#a14f38;font-size:.82rem}.queue-track[aria-current=true] .queue-indicator:after{content:'NOW PLAYING'}@keyframes tape-in{from{transform:translateY(-115px) rotate(-5deg);opacity:.35}to{transform:translateY(0) rotate(0);opacity:1}}@keyframes reel-spin{to{transform:rotate(360deg)}}@media(prefers-reduced-motion:reduce){.cassette{animation:none}.player-deck[data-playing=true] .cassette-reel{animation:none}}@media(max-width:720px){.listening-layout{grid-template-columns:1fr}.note-panel{min-height:200px}.cassette{grid-template-columns:1fr 40px 40px;gap:7px;padding:9px}.cassette-reel{width:36px;height:36px}.queue-heading{align-items:center}}"
  <> "input,textarea{width:100%;font:inherit;color:#222019;background:#fff;border:1px solid #dcd4c6;border-radius:10px;padding:13px 14px;outline:none}input:focus,textarea:focus{border-color:#a14f38;box-shadow:0 0 0 3px #a14f381c}textarea{resize:vertical}.track-fields .track-title,.track-fields textarea{margin-top:9px}"
  <> ".button{display:inline-flex;align-items:center;justify-content:center;background:#a14f38;border:0;border-radius:999px;color:white;text-decoration:none;font:600 .95rem 'DM Sans',sans-serif;padding:14px 22px;cursor:pointer}.button:hover{background:#843d2b}.tracks-heading{display:flex;align-items:center;justify-content:space-between;margin:32px 0 12px}.quiet-button,.icon-button{border:0;background:transparent;color:#a14f38;font:600 .9rem 'DM Sans',sans-serif;cursor:pointer}.track-row{display:grid;grid-template-columns:34px 1fr auto;gap:12px;padding:16px 0;border-top:1px solid #eee7db}.track-number{color:#a14f38;padding-top:12px}.track-fields{min-width:0}.url-hint,.fine-print{font-size:.78rem;color:#827a6e;margin:7px 0}.url-valid{color:#47734d}.url-invalid{color:#a14f38}.row-actions{display:flex;flex-direction:column;gap:2px}.icon-button{font-size:1.1rem;color:#766c5d;padding:6px}.remove-button{color:#a14f38}.submit-button{margin-top:18px}.error{padding:12px 14px;border-radius:9px;background:#fbebe6;color:#923c2b}.share-box{display:flex;gap:10px;margin:28px 0 12px}.share-box input{flex:1}.text-link{display:inline-block;color:#a14f38;font-weight:600;text-decoration:none;margin:14px 18px 0 0}.secondary-link{color:#716b61}.success-card h1{font-size:clamp(2.5rem,6vw,3.8rem)}.tape-art{position:absolute;right:2%;bottom:9%;width:210px;height:132px;background:#cb6548;border:7px solid #24211d;border-radius:12px;transform:rotate(-8deg);box-shadow:14px 18px 0 #dbd0be}.reel{position:absolute;top:27px;width:56px;height:56px;border:8px solid #29251f;border-radius:50%;background:repeating-conic-gradient(#29251f 0 12deg,#f0d6ad 12deg 25deg)}.reel-one{left:27px}.reel-two{right:27px}.shared-tracklist{padding-left:26px}.shared-track{padding:16px 10px;border-top:1px solid #eee7db}.track-note-display{background:#f7f1e7;border-radius:10px;padding:14px;line-height:1.6}@media(max-width:600px){.tape-art{position:relative;right:auto;bottom:auto;margin:50px auto 0;transform:rotate(-6deg)}.share-box{flex-direction:column}.row-actions{gap:0}.track-row{grid-template-columns:25px 1fr auto;gap:6px}}"

trackEditorScript :: Text
trackEditorScript = T.unlines
  [ "function youtubeVideoId(value){try{const url=new URL(value),host=url.hostname.toLowerCase(),parts=url.pathname.split('/').filter(Boolean);let id='';if(['youtube.com','www.youtube.com','m.youtube.com','music.youtube.com'].includes(host)){if(parts[0]==='watch')id=url.searchParams.get('v')||'';else if(['shorts','embed','live'].includes(parts[0]))id=parts[1]||''}else if(['youtu.be','www.youtu.be'].includes(host))id=parts[0]||'';return ['https:','http:'].includes(url.protocol)&&/^[A-Za-z0-9_-]{11}$/.test(id)?id:''}catch(_){return ''}}"
  , "const pendingTitleLookups=new Map();"
  , "function lookupTitle(input,id){const row=input.closest('.track-row'),titleField=row.querySelector('.track-title'),hint=row.querySelector('.url-hint'),version=Number(input.dataset.lookupVersion||0)+1;input.dataset.lookupVersion=String(version);input.dataset.lookupUrl=input.value;titleField.value='';hint.textContent='Looking up YouTube title…';const endpoint=new URL('https://www.youtube.com/oembed');endpoint.searchParams.set('url','https://www.youtube.com/watch?v='+id);endpoint.searchParams.set('format','json');let request=fetch(endpoint,{mode:'cors',credentials:'omit',signal:AbortSignal.timeout(8000)}).then(response=>{if(!response.ok)throw new Error('Title lookup failed');return response.json()}).then(data=>{if(Number(input.dataset.lookupVersion)!==version)return;if(typeof data.title==='string'&&data.title.trim()){titleField.value=data.title.trim();hint.textContent='YouTube title found. You can edit it.'}else{hint.textContent='Title unavailable; you can enter one yourself.'}}).catch(()=>{if(Number(input.dataset.lookupVersion)===version)hint.textContent='Could not load the title; you can enter one yourself.'}).finally(()=>{if(pendingTitleLookups.get(input)===request)pendingTitleLookups.delete(input)});pendingTitleLookups.set(input,request);return request}"
  , "document.addEventListener('input',function(event){if(!event.target.matches('.video-url'))return;const input=event.target,row=input.closest('.track-row'),titleField=row.querySelector('.track-title'),hint=row.querySelector('.url-hint');if(input.value!==input.dataset.lookupUrl)titleField.value='';hint.textContent=input.value?(youtubeVideoId(input.value)?'YouTube link looks good.':'Use a youtube.com or youtu.be video link.'):'YouTube links from youtube.com or youtu.be';hint.classList.toggle('url-valid',Boolean(input.value&&youtubeVideoId(input.value)));hint.classList.toggle('url-invalid',Boolean(input.value&&!youtubeVideoId(input.value)))});"
  , "document.addEventListener('change',function(event){if(!event.target.matches('.video-url'))return;const id=youtubeVideoId(event.target.value);if(id)lookupTitle(event.target,id)});"
  , "const createForm=document.querySelector('form[action=\"/create\"]');createForm.addEventListener('submit',function(event){const inputs=Array.from(createForm.querySelectorAll('.video-url'));const requests=[];for(const input of inputs){const id=youtubeVideoId(input.value);if(id&&input.dataset.lookupUrl!==input.value)requests.push(lookupTitle(input,id))}requests.push(...pendingTitleLookups.values());if(requests.length){event.preventDefault();Promise.all(requests).then(()=>createForm.requestSubmit(event.submitter))}});"
  , "document.addEventListener('click',function(event){const add=event.target.closest('#add-track');if(add){const list=document.querySelector('#track-list'),row=document.querySelector('#track-template').content.firstElementChild.cloneNode(true);list.append(row);row.querySelector('input').focus();return}const move=event.target.closest('[data-move]');if(move){const row=move.closest('.track-row');if(move.dataset.move==='up'&&row.previousElementSibling)row.parentNode.insertBefore(row,row.previousElementSibling);if(move.dataset.move==='down'&&row.nextElementSibling)row.parentNode.insertBefore(row.nextElementSibling,row);return}const remove=event.target.closest('[data-remove]');if(remove){const rows=document.querySelectorAll('#track-list .track-row');if(rows.length>1)remove.closest('.track-row').remove()}});"
  , "document.body.addEventListener('htmx:responseError',function(){document.querySelector('#create-flow').insertAdjacentHTML('afterbegin','<p class=error role=alert>We could not save this mixtape. Please try again.</p>')});"
  ]
copyScript :: Text
copyScript = "const input=document.querySelector('#share-link');input.value=new URL(input.value,location.origin).href;document.addEventListener('click',async function(e){const button=e.target.closest('[data-copy]');if(!button)return;try{await navigator.clipboard.writeText(input.value);document.querySelector('#copy-status').textContent='Link copied to clipboard.'}catch(_){input.select();document.execCommand('copy');document.querySelector('#copy-status').textContent='Link copied.'}});"

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

playbackScript :: Text
playbackScript = "(function(){const queue=Array.from(document.querySelectorAll('#track-queue [data-track-index]'));if(!queue.length)return;let current=0,player=null,ready=false;const deck=document.querySelector('#player-deck'),status=document.querySelector('#playback-status'),playButton=document.querySelector('#toggle-playback'),startButton=document.querySelector('#start-playback');function announce(message){status.textContent=message}function showTrack(){const item=queue[current];document.querySelector('#current-track-title').textContent=item.dataset.trackTitle;document.querySelector('#current-track-artist').textContent=item.dataset.trackArtist||'';document.querySelector('#current-track-note').textContent=item.dataset.trackNote;document.querySelector('#current-track-counter').textContent=(current+1)+' / '+queue.length;queue.forEach((row,index)=>{const active=index===current;row.setAttribute('aria-current',active?'true':'false');row.querySelector('.queue-indicator').textContent=active?'♪':''});document.querySelector('#previous-track').disabled=current===0;document.querySelector('#next-track').disabled=current===queue.length-1}function selectTrack(index){if(index<0||index>=queue.length)return;current=index;showTrack();startButton.hidden=true;if(ready)player.loadVideoById(queue[current].dataset.videoId)}showTrack();queue.forEach(row=>row.addEventListener('click',()=>selectTrack(Number(row.dataset.trackIndex))));document.querySelector('#previous-track').addEventListener('click',()=>{if(current===0){if(ready)player.seekTo(0,true);return}selectTrack(current-1)});document.querySelector('#next-track').addEventListener('click',()=>selectTrack(current+1));function play(){if(ready){player.playVideo();startButton.hidden=true;announce('Starting playback…')}}function toggle(){if(!ready)return;if(player.getPlayerState()===YT.PlayerState.PLAYING){player.pauseVideo()}else play()}playButton.addEventListener('click',toggle);startButton.addEventListener('click',play);window.onYouTubeIframeAPIReady=function(){player=new YT.Player('youtube-player',{width:1280,height:720,videoId:queue[0].dataset.videoId,playerVars:{autoplay:1,controls:1,playsinline:1,rel:0,origin:window.location.origin},events:{onReady:function(event){ready=true;announce('Starting the first track…');if(current===0)event.target.playVideo();else event.target.loadVideoById(queue[current].dataset.videoId)},onStateChange:function(event){if(event.data===YT.PlayerState.PLAYING){deck.dataset.playing='true';playButton.textContent='Pause';startButton.hidden=true;announce('Now playing '+queue[current].dataset.trackTitle)}else if(event.data===YT.PlayerState.PAUSED){deck.dataset.playing='false';playButton.textContent='Play';announce('Paused')}else if(event.data===YT.PlayerState.ENDED){deck.dataset.playing='false';if(current<queue.length-1)selectTrack(current+1);else{playButton.textContent='Play';announce('You reached the end of the tape.')}}},onAutoplayBlocked:function(){deck.dataset.playing='false';playButton.textContent='Play';startButton.hidden=false;announce('Your browser blocked autoplay. Press Start listening to play the tape.')},onError:function(){deck.dataset.playing='false';playButton.textContent='Play';startButton.hidden=false;announce('This video cannot be played here. Try another track.')}}});};const api=document.createElement('script');api.src='https://www.youtube.com/iframe_api';api.async=true;api.onerror=function(){announce('The YouTube player did not load. Check your connection and refresh to try again.')};document.head.appendChild(api)})();"
