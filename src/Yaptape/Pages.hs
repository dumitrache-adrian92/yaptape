module Yaptape.Pages
  ( assetVersion
  , computeAssetVersion
  , renderLandingPage
  , renderCreatePage
  , renderCreatedPage
  , renderMixtapePage
  , renderErrorPage
  ) where

import Yaptape.Pages.Create (renderCreatePage, renderCreatedPage)
import Yaptape.Pages.Layout (assetVersion, computeAssetVersion, renderErrorPage, renderLandingPage)
import Yaptape.Pages.Player (renderMixtapePage)
