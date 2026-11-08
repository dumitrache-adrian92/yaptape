module Yaptape.Pages
  ( renderLandingPage
  , renderCreatePage
  , renderCreatedPage
  , renderMixtapePage
  , renderErrorPage
  ) where

import Yaptape.Pages.Create (renderCreatePage, renderCreatedPage)
import Yaptape.Pages.Layout (renderErrorPage, renderLandingPage)
import Yaptape.Pages.Player (renderMixtapePage)
