module Handler.Entry where

import Import

entryForm :: Form Entry
entryForm = renderTable $ Entry
  <$> areq textField "Title" Nothing
  <*> areq textareaField "Content" Nothing

prepEntryForm :: Entry -> Form Entry
prepEntryForm entry = renderTable $ Entry
  <$> areq textField "Title" (Just $ entryName entry)
  <*> areq textareaField "Content" (Just $ entryContent entry)

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  entry <- runDB $ get404 entryId
  (entryWidget, enctype) <- generateFormPost $ prepEntryForm entry
  defaultLayout $ do
    setTitle $ toHtml $ entryName $ entry
    $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
  ((res,entryWidget), enctype) <- runFormPost entryForm
  entry <- runDB $ get404 entryId
  case res of
    FormSuccess entryData -> do
      runDB $ replace entryId entryData
      setMessage $ toHtml $ (entryName entry) <> " created"
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitle "Bad."
      $(widgetFile "error")

