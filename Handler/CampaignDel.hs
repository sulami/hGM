module Handler.CampaignDel where

import Import

getCampaignDelR :: CampaignId -> Handler Html
getCampaignDelR cid = do
  user <- requireAuthId
  camp <- runDB $ get404 cid
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      runDB $ deleteWhere [EntryCampaignId ==. cid]
      runDB $ delete cid
      setMessage . toHtml $ campaignName camp <> " deleted."
      redirect HomeR

