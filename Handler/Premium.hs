module Handler.Premium where

import Import
import Import.Premium (prizes)

getPremiumR :: Handler Html
getPremiumR = do
  _ <- requireAuth
  let periods = prizes
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = error "Not yet implemented: postPremiumR"

