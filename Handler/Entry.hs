module Handler.Entry where

import Import

getEntryR :: EntryId -> Handler Html
getEntryR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  if user /= entryOwnerId entry
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      defaultLayout $ do
        setTitle $ toHtml $ entryName entry
        $(widgetFile "entry")

