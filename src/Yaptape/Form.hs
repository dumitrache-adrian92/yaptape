{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Yaptape.Form
  ( CreateMixtapeForm (..)
  , createMixtapeFromForm
  ) where

import Data.List (zip4)
import Data.Text (Text)
import Web.FormUrlEncoded (FromForm (..), lookupAll, lookupMaybe, lookupUnique)
import Yaptape.Domain (Mixtape, mkMixtape, mkTrack)
import Yaptape.YouTube (parseYouTubeUrl)

data CreateMixtapeForm = CreateMixtapeForm
  { formTitle :: Text
  , formDescription :: Maybe Text
  , formVideoUrls :: [Text]
  , formTitles :: [Text]
  , formArtists :: [Text]
  , formNotes :: [Text]
  } deriving (Show, Eq)

instance FromForm CreateMixtapeForm where
  fromForm form = CreateMixtapeForm
    <$> lookupUnique "title" form
    <*> lookupMaybe "description" form
    <*> pure (lookupAll "videoUrls" form)
    <*> pure (lookupAll "titles" form)
    <*> pure (lookupAll "artists" form)
    <*> pure (lookupAll "notes" form)

createMixtapeFromForm :: CreateMixtapeForm -> Either Text Mixtape
createMixtapeFromForm form
  | null form.formNotes = Left "Add at least one track."
  | length form.formVideoUrls /= length form.formNotes = Left "Each track needs a YouTube link and a note."
  | length form.formTitles > length form.formNotes = Left "Track titles must match the tracklist."
  | length form.formArtists > length form.formNotes = Left "Track artists must match the tracklist."
  | otherwise = do
      parsedTracks <- traverse makeTrack (zip4 form.formVideoUrls form.formNotes (form.formTitles <> repeat "") (form.formArtists <> repeat ""))
      mkMixtape form.formTitle form.formDescription parsedTracks
  where
    makeTrack (url, trackNote, trackTitle, trackArtist) = do
      parsedVideoId <- parseYouTubeUrl url
      mkTrack parsedVideoId (Just trackTitle) (Just trackArtist) trackNote
