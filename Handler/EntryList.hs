module Handler.EntryList where

import Import

getEntryListR :: CampaignId -> Handler Html
getEntryListR cid = do
  user <- requireAuthId
  camp <- runDB $ get404 cid
  entries <- runDB $ selectList [EntryCampaignId ==. cid] [Asc EntryId]
  defaultLayout $ do
    setTitle "Entries"
    $(widgetFile "entries")

