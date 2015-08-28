module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  user <- requireAuthId
  camps <- runDB $ selectList [CampaignOwnerId ==. user] [Asc CampaignName]
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

