module Handler.Premium where

import Import

getPremiumR :: Handler Html
getPremiumR = do
  _ <- requireAuth
  let periods = [ ( 1,  2.99)
                , ( 3,  6.99)
                , ( 6, 12.99)
                , (12, 24.99)
                , (24, 44.99)
                ] :: [(Int, Float)]
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = error "Not yet implemented: postPremiumR"

