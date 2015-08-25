module Handler.Entry where

import Import

import Data.Maybe (fromJust)

entryForm :: Form Entry
entryForm = renderDivs $ Entry
  <$> areq textField "Title"    Nothing
  <*> areq textField "Content"  Nothing

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  mbEntry <- runDB $ get entryId
  (entryWidget, enctype) <- generateFormPost entryForm
  case mbEntry of
    Nothing -> defaultLayout $ do
                  $(widgetFile "newentry")
    _ -> defaultLayout $ do
            let entry = fromJust mbEntry
            setTitle $ toHtml $ entryName $ entry
            $(widgetFile "entry")

postEntryR :: EntryId -> Handler Html
postEntryR entryId = do
  ((res,entryWidget), enctype) <- runFormPost entryForm
  case res of
    FormSuccess entry -> do
      entryId <- runDB $ insert entry
      setMessage $ toHtml $ (entryName entry) <> " created"
      redirect $ EntryR entryId
    _ -> defaultLayout $ do
      setTitle "Bad."
      $(widgetFile "error")

