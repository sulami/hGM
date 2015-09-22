module Handler.EntryNew where

import           Import

import           Handler.EntryEdit (entryForm, updateRelationships)

getEntryNewR :: CampaignId -> Handler Html
getEntryNewR cid = do
  _ <- requireAuthId
  camp <- runDB $ get404 cid
  (entryWidget, enctype) <- generateFormPost $ entryForm cid
  defaultLayout $ do
    setTitle "New Entry"
    $(widgetFile "entrynew")

postEntryNewR :: CampaignId -> Handler Html
postEntryNewR cid = do
  _ <- requireAuthId
  ((res,_), _) <- runFormPost $ entryForm cid
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      updateRelationships cid
      setMessage . toHtml $ entryName entry <> " created"
      redirect . EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

