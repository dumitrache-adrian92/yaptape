module Yaptape.Pages
  ( assetVersion
  , configureAssetVersion
  , computeAssetVersion
  , renderLandingPage
  , renderCreatePage
  , renderCreatedPage
  , renderMixtapePage
  , renderErrorPage
  ) where

import Yaptape.Pages.Create (renderCreatePage, renderCreatedPage)
import Yaptape.Pages.Layout (assetVersion, computeAssetVersion, configureAssetVersion, renderErrorPage, renderLandingPage)
import Yaptape.Pages.Player (renderMixtapePage)
