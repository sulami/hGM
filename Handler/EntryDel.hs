module Handler.EntryDel where

import Import

getEntryDelR :: EntryId -> Handler Html
getEntryDelR entryId = do
  user <- requireAuthId
  runDB $ delete entryId
  setMessage "Entry deleted."
  redirect HomeR

