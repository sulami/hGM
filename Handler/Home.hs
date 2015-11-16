module Handler.Home where

import Import
import Import.Premium (hasPremium)

getHomeR :: Handler Html
getHomeR = do
  creds <- maybeAuth
  case creds of
    Nothing -> emptyLayout $ do
      setTitle "Welcome"
      $(widgetFile "login")
    Just (Entity uid user) -> do
      prem <- hasPremium user
      camps <- runDB $ selectList [CampaignOwnerId ==. uid] [Asc CampaignName]
      let newenabled = prem || null camps
      defaultLayout $ do
        setTitle "Home"
        $(widgetFile "homepage")

-- | Render only the most basic HTML wrapper needed, mainly for the landing
-- page
emptyLayout :: Widget -> Handler Html
emptyLayout widget = do
  pc <- widgetToPageContent $ do
    addStylesheetRemote "http://semantic-ui.com/dist/semantic.min.css"
    widget
  withUrlRenderer
    [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          <meta charset=utf-8>
          ^{pageHead pc}
        <body>
          ^{pageBody pc}
    |]

