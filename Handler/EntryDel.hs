module Handler.EntryDel where

import Import

getEntryDelR :: EntryId -> Handler Html
getEntryDelR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  if user /= entryOwnerId entry
    then
      defaultLayout $ $(widgetFile "error")
    else do
      runDB $ delete entryId
      setMessage "Entry deleted."
      redirect HomeR

