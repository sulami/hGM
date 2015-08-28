module Handler.NewEntry where

import Import

import Handler.EntryEdit (entryForm)

getNewEntryR :: CampaignId -> Handler Html
getNewEntryR cid = do
  _ <- requireAuthId
  _ <- runDB $ get404 cid
  (entryWidget, enctype) <- generateFormPost $ entryForm cid
  defaultLayout $(widgetFile "newentry")

postNewEntryR :: CampaignId -> Handler Html
postNewEntryR cid = do
  _ <- requireAuthId
  ((res,_), _) <- runFormPost $ entryForm cid
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessage $ toHtml $ entryName entry <> " created"
      redirect $ EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

