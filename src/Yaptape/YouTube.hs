module Yaptape.YouTube
  ( YouTubeVideoId
  , YouTubeVideoIdError (..)
  , YouTubeUrlSpec (..)
  , youTubeUrlSpec
  , mkYouTubeVideoId
  , parseYouTubeUrl
  , unYouTubeVideoId
  , renderYouTubeVideoIdError
  ) where

import Yaptape.YouTube.Internal
  ( YouTubeVideoId
  , YouTubeVideoIdError (..)
  , YouTubeUrlSpec (..)
  , mkYouTubeVideoId
  , parseYouTubeUrl
  , renderYouTubeVideoIdError
  , unYouTubeVideoId
  , youTubeUrlSpec
  )
