module Handler.CategoryDel where

import           Import

getCategoryDelR :: CategoryId -> Handler Html
getCategoryDelR categoryId = do
  user <- requireAuthId
  category <- runDB $ get404 categoryId
  let campaignId = categoryCampaignId category
  camp <- runDB $ get404 campaignId
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      entries <- runDB $ selectList [ EntryCampaignId ==. campaignId,
                                      EntryCategoryId ==. Just categoryId ] []
      forM_ entries $ \(Entity k _) ->
        runDB $ update k [EntryCategoryId =. Nothing]
      runDB $ delete categoryId
      setMessage . toHtml $ "Cateogry " <> categoryName category <> " deleted."
      redirect . EntriesR $ CampaignOptsR campaignId

