module Handler.EntryList where

import           Import
import           Import.Premium (hasPremium)

getEntryListR :: CampaignId -> Handler Html
getEntryListR cid = do
  Entity uid user <- requireAuth
  camp <- runDB $ get404 cid
  entries <- runDB $ selectList [EntryCampaignId ==. cid] [Asc EntryName]
  handouts <- runDB $ selectList [HandoutCampaignId ==. cid] [Asc HandoutName]
  prem <- hasPremium user
  defaultLayout $ do
    setTitle . toHtml $ campaignName camp
    $(widgetFile "entrylist")

