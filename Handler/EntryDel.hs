module Handler.EntryDel where

import           Import

import           Handler.EntryEdit (updateRelationships)

getEntryDelR :: EntryId -> Handler Html
getEntryDelR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  camp <- runDB . get404 $ entryCampaignId entry
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      runDB $ delete entryId
      updateRelationships $ entryCampaignId entry
      setMessage "Entry deleted."
      redirect . EntriesR . EntryListR $ entryCampaignId entry

