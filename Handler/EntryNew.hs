module Handler.EntryNew where

import           Import

import           Handler.EntryEdit (entryForm, formatCategories,
                                    updateRelationships)

getEntryNewR :: CampaignId -> Handler Html
getEntryNewR cid = do
  Entity uid _ <- requireAuth
  camp <- runDB $ get404 cid
  unless (uid == campaignOwnerId camp) $ redirect HomeR
  categories <- runDB $ selectList [CategoryCampaignId ==. cid]
                  [Asc CategoryName]
  (entryWidget, enctype) <- generateFormPost . entryForm cid $
                              formatCategories categories
  defaultLayout $ do
    setTitle "New Entry"
    $(widgetFile "entrynew")

postEntryNewR :: CampaignId -> Handler Html
postEntryNewR cid = do
  _ <- requireAuthId
  categories <- runDB $ selectList [CategoryCampaignId ==. cid]
                  [Asc CategoryName]
  ((res,_), _) <- runFormPost . entryForm cid $ formatCategories categories
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      updateRelationships cid
      setMessage . toHtml $ entryName entry <> " created"
      redirect . EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

