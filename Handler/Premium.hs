module Handler.Premium where

import Import

getPremiumR :: Handler Html
getPremiumR = do
  Entity uid user <- requireAuth
  when (userPremium user) . redirect $ AccountR OverviewR
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = error "Not yet implemented: postPremiumR"

