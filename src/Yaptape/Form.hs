{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Form
  ( CreateMixtapeForm (..)
  , createMixtapeFromForm
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI (..), URIAuth (..), parseURI)
import Web.FormUrlEncoded (FromForm (..), lookupAll, lookupMaybe, lookupUnique)
import Yaptape.Domain (Mixtape (..), Track (Track))
import Yaptape.YouTube (YouTubeVideoId, mkYouTubeVideoId)

data CreateMixtapeForm = CreateMixtapeForm
  { formTitle :: Text
  , formDescription :: Maybe Text
  , formVideoUrls :: [Text]
  , formTitles :: [Text]
  , formNotes :: [Text]
  } deriving (Show, Eq)

instance FromForm CreateMixtapeForm where
  fromForm form = CreateMixtapeForm
    <$> lookupUnique "title" form
    <*> lookupMaybe "description" form
    <*> pure (lookupAll "videoUrls" form)
    <*> pure (lookupAll "titles" form)
    <*> pure (lookupAll "notes" form)

createMixtapeFromForm :: CreateMixtapeForm -> Either Text Mixtape
createMixtapeFromForm form
  | T.null (T.strip form.formTitle) = Left "Give your mixtape a title."
  | null form.formNotes = Left "Add at least one track."
  | length form.formVideoUrls /= length form.formNotes = Left "Each track needs a YouTube link and a note."
  | otherwise = do
      parsedTracks <- traverse makeTrack (zip3 form.formVideoUrls form.formNotes (form.formTitles <> repeat ""))
      pure (Mixtape (T.strip form.formTitle) (nonEmpty form.formDescription) parsedTracks)
  where
    makeTrack (url, trackNote, trackTitle)
      | T.null (T.strip trackNote) = Left "Add a note for every track."
      | otherwise = do
          parsedVideoId <- parseYouTubeUrl url
          pure (Track parsedVideoId (nonEmpty (Just trackTitle)) Nothing (T.strip trackNote))

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty = (>>= \value -> if T.null (T.strip value) then Nothing else Just (T.strip value))

parseYouTubeUrl :: Text -> Either Text YouTubeVideoId
parseYouTubeUrl raw = do
  uri <- maybe invalid Right (parseURI (T.unpack (T.strip raw)))
  authority <- maybe invalid Right (uriAuthority uri)
  let host = T.toLower (T.pack (uriRegName authority))
      pathParts = filter (not . T.null) (T.splitOn "/" (T.pack (uriPath uri)))
      candidate
        | uriScheme uri `elem` ["https:", "http:"] && host `elem` ["youtube.com", "www.youtube.com", "m.youtube.com", "music.youtube.com"] =
            case pathParts of
              ["watch"] -> queryValue "v" (T.pack (uriQuery uri))
              [kind, video] | kind `elem` ["shorts", "embed", "live"] -> Just video
              _ -> Nothing
        | uriScheme uri `elem` ["https:", "http:"] && host `elem` ["youtu.be", "www.youtu.be"] =
            case pathParts of
              [video] -> Just video
              _ -> Nothing
        | otherwise = Nothing
  videoText <- maybe invalid Right candidate
  either (const invalid) Right (mkYouTubeVideoId videoText)
  where
    invalid = Left "Enter a YouTube video link (youtube.com/watch?v=… or youtu.be/…)."

queryValue :: Text -> Text -> Maybe Text
queryValue key = lookup key . map (\field -> let (name, value) = T.breakOn "=" field in (name, T.drop 1 value)) . T.splitOn "&" . T.drop 1
