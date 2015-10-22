module Handler.HandoutShare where

import Import
import Database.Persist.Sql (fromSqlKey)

getHandoutShareR :: HandoutId -> Handler Text
getHandoutShareR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  camp <- runDB . get404 $ handoutCampaignId handout
  if user == campaignOwnerId camp
    then generateLink handoutId
    else permissionDenied "Permission Denied"

generateLink :: HandoutId -> Handler Text
generateLink hid = do
  app <- getYesod
  let root = appRoot $ appSettings app
      uniq = pack . show . (*23) $ fromSqlKey hid
  return $ root ++ "/shared/" ++ uniq

