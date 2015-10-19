module Handler.Premium where

import Import

getPremiumR :: Handler Html
getPremiumR = do
  Entity uid user <- requireAuth
  let periods = [1,3,6,12,24] :: [Int]
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = error "Not yet implemented: postPremiumR"

prize :: Int -> Int
prize m = m * 5 - (m - 1)

