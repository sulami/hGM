module Handler.DelCampaign where

import Import

getDelCampaignR :: CampaignId -> Handler Html
getDelCampaignR cid = do
  user <- requireAuthId
  camp <- runDB $ get404 cid
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      runDB $ deleteWhere [EntryCampaignId ==. cid]
      runDB $ delete cid
      setMessage "Campaign deleted."
      redirect HomeR

