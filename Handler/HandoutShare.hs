module Handler.HandoutShare where

import Import

getHandoutShareR :: HandoutId -> Handler Text
getHandoutShareR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  camp <- runDB . get404 $ handoutCampaignId handout
  if user == campaignOwnerId camp
    then return $ generateLink handoutId
    else permissionDenied "Permission Denied"

generateLink :: HandoutId -> Text
generateLink _ = "abc"

