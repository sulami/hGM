module Handler.HandoutDel where

import Import

getHandoutDelR :: HandoutId -> Handler Html
getHandoutDelR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  camp <- runDB . get404 $ handoutCampaignId handout
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      runDB $ delete handoutId
      setMessage "handout deleted."
      redirect . EntriesR . EntryListR $ handoutCampaignId handout

