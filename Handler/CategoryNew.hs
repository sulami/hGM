module Handler.CategoryNew where

import           Import

postCategoryNewR :: CampaignId -> Handler Html
postCategoryNewR campaignId = do
  user <- requireAuthId
  camp <- runDB $ get404 campaignId
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      newCatName <- lookupPostParam "catname"
      case newCatName of
        Nothing   -> do
          setMessage "Internal error."
          defaultLayout $ $(widgetFile "error")
        Just name -> do
          _ <- runDB . insert $ Category name campaignId
          redirect . EntriesR $ CampaignOptsR campaignId

