module Handler.Entry where

import Import

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  camp <- runDB . get404 $ entryCampaignId entry
  inThis <- runDB $ selectList [EntryId <-. entryInThis entry] []
  thisIn <- runDB $ selectList [EntryId <-. entryThisIn entry] []
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      defaultLayout $ do
        setTitle . toHtml $ entryName entry
        $(widgetFile "entry")

