module Handler.Handout where

import Import

getHandoutR :: HandoutId -> Handler Html
getHandoutR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  camp <- runDB . get404 $ handoutCampaignId handout
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      defaultLayout $ do
        setTitle . toHtml $ handoutName handout
        $(widgetFile "handout")

