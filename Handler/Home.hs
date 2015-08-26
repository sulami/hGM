module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  user <- requireAuthId
  numEntries <- runDB $ count [EntryOwnerId ==. user]
  defaultLayout $ do
    setTitle "Home"
    $(widgetFile "homepage")

