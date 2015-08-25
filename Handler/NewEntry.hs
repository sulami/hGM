module Handler.NewEntry where

import Import

import Handler.EntryEdit (entryForm)

getNewEntryR :: Handler Html
getNewEntryR = do
  user <- requireAuthId
  (entryWidget, enctype) <- generateFormPost $ entryForm user
  defaultLayout $(widgetFile "newentry")

postNewEntryR :: Handler Html
postNewEntryR = do
  user <- requireAuthId
  ((res,_), _) <- runFormPost $ entryForm user
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessage $ toHtml $ (entryName entry) <> " created"
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitle "Bad."
      $(widgetFile "error")

