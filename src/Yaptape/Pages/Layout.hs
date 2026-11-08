{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Pages.Layout
  ( PageMeta (..)
  , defaultMeta
  , assetVersion
  , configureAssetVersion
  , computeAssetVersion
  , versionedAsset
  , layout
  , layoutWithMeta
  , renderLandingPage
  , renderErrorPage
  ) where

import Control.Exception (IOException, try)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Lucid
import Lucid.Base (makeAttribute)
import Numeric (showHex)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)

trackedAssets :: [FilePath]
trackedAssets =
  [ "css/app.css"
  , "js/copy-link.js"
  , "js/create.js"
  , "js/playback.js"
  ]

-- | Compute a deterministic 64-bit FNV-1a hex digest over the static asset files in a directory.
computeAssetVersion :: FilePath -> IO Text
computeAssetVersion staticDir = do
  chunks <- traverse readAsset trackedAssets
  let digest = BS.foldl' (\acc b -> fnv1aStep acc (fromIntegral b)) fnvOffsetBasis (BS.concat chunks)
  pure (T.pack (showHex digest ""))
  where
    readAsset relPath = do
      res <- try (BS.readFile (staticDir </> relPath)) :: IO (Either IOException BS.ByteString)
      pure (either (const BS.empty) id res)
    fnvOffsetBasis :: Word64
    fnvOffsetBasis = 0xcbf29ce484222325
    fnvPrime :: Word64
    fnvPrime = 0x100000001b3
    fnv1aStep :: Word64 -> Word64 -> Word64
    fnv1aStep acc byte = (acc `xor` byte) * fnvPrime

{-# NOINLINE assetVersionRef #-}
assetVersionRef :: IORef Text
assetVersionRef = unsafePerformIO (computeAssetVersion "static" >>= newIORef)

-- | Recompute and store the global asset version hash from the configured static directory.
configureAssetVersion :: FilePath -> IO ()
configureAssetVersion staticDir = computeAssetVersion staticDir >>= writeIORef assetVersionRef

{-# NOINLINE assetVersion #-}
assetVersion :: Text
assetVersion = unsafePerformIO (readIORef assetVersionRef)

versionedAsset :: Text -> Text
versionedAsset path = path <> "?v=" <> assetVersion

data PageMeta = PageMeta
  { metaTitle :: Text
  , metaDescription :: Maybe Text
  , metaImage :: Maybe Text
  , metaOgType :: Maybe Text
  }

defaultMeta :: Text -> PageMeta
defaultMeta pageTitle = PageMeta
  { metaTitle = pageTitle
  , metaDescription = Just "Make a mixtape from YouTube tracks, add a note to each track, and share it."
  , metaImage = Nothing
  , metaOgType = Just "website"
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
    link_ [rel_ "stylesheet", href_ (versionedAsset "/assets/css/app.css")]
  body_ content

renderLandingPage :: Html ()
renderLandingPage = layout "Yaptape" $ main_ [class_ "landing"] $ do
  p_ [class_ "eyebrow"] "A LITTLE MORE TO THE MUSIC"
  h1_ "Make a mixtape that says a little more."
  p_ [class_ "intro"] "Bring together the songs you love, annotate each one, and send someone a tape they can listen through."
  p_ [class_ "intro"] "Use Yaptape to introduce someone to a new genre, confess your love, reminisce through music or tell a story separated by tracks."
  a_ [href_ "/create", class_ "button"] "Make a mixtape"
  div_ [class_ "tape-art", makeAttribute "aria-hidden" "true"] $ do
    div_ [class_ "reel reel-one"] mempty
    div_ [class_ "reel reel-two"] mempty
  footer_ [class_ "github-link"] $ a_ [href_ "https://github.com/dumitrache-adrian92/yaptape/tree/main"] "This site is open source, check it out!"

renderErrorPage :: Text -> Text -> Html ()
renderErrorPage heading message = layout (heading <> " · Yaptape") $ main_ [class_ "page-shell"] $ do
  a_ [href_ "/", class_ "wordmark"] "yaptape"
  section_ [class_ "create-card"] $ do
    p_ [class_ "eyebrow"] "SOMETHING WENT WRONG"
    h1_ (toHtml heading)
    p_ [class_ "muted"] (toHtml message)
    a_ [href_ "/", class_ "button"] "Back to Yaptape"
