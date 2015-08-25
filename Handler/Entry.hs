module Handler.Entry where

import Import

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  entry <- runDB $ get404 entryId
  defaultLayout $ do
    setTitle $ toHtml $ entryName entry
    $(widgetFile "entry")

