module Handler.Home where

import Import
import Import.Premium (hasPremium)

getHomeR :: Handler Html
getHomeR = do
  creds <- maybeAuth
  case creds of
    Nothing -> defaultLayout $ do
      setTitle "Welcome"
      $(widgetFile "login")
    Just (Entity uid user) -> do
      prem <- hasPremium user
      camps <- runDB $ selectList [CampaignOwnerId ==. uid] [Asc CampaignName]
      let newenabled = prem || null camps
      defaultLayout $ do
        setTitle "Home"
        $(widgetFile "homepage")

