module Handler.CategoryDel where

import           Import

getCategoryDelR :: CategoryId -> Handler Html
getCategoryDelR categoryId = do
  _ <- requireAuth
  category <- runDB $ get404 categoryId
  let campaignId = categoryCampaignId category
  entries <- runDB $ selectList [ EntryCampaignId ==. campaignId,
                                  EntryCategoryId ==. Just categoryId ] []
  forM_ entries $ \(Entity k _) -> runDB $ update k [EntryCategoryId =. Nothing]
  runDB $ delete categoryId
  redirect . EntriesR $ CampaignOptsR campaignId

