module Handler.Home where

import Import
import Import.Premium (hasPremium)

getHomeR :: Handler Html
getHomeR = do
  Entity uid user <- requireAuth
  prem <- hasPremium user
  camps <- runDB $ selectList [CampaignOwnerId ==. uid] [Asc CampaignName]
  let newenabled = prem || null camps
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

