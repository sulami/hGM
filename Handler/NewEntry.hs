module Handler.NewEntry where

import Import

entryForm :: Form Entry
entryForm = renderDivs $ Entry
  <$> areq textField "Title"    Nothing
  <*> areq textField "Content"  Nothing

getNewEntryR :: Handler Html
getNewEntryR = do
  (entryWidget, enctype) <- generateFormPost entryForm
  defaultLayout $(widgetFile "newentry")

postNewEntryR :: Handler Html
postNewEntryR = do
  ((res,entryWidget), enctype) <- runFormPost entryForm
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessage $ toHtml $ (entryName entry) <> " created"
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitle "Bad."
      $(widgetFile "error")

