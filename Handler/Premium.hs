module Handler.Premium where

import Import
import Import.Premium (prizes)
import Secret

getPremiumR :: Handler Html
getPremiumR = do
  Entity uid user <- requireAuth
  let periods = prizes
      pubKey = stripePublicKey
      userAddr = userIdent user
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = error "Not yet implemented: postPremiumR"

-- | Convert a float prize to an in-cents int
stripePrize :: Float -> Int
stripePrize = round . (* 100)

