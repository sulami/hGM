module Handler.EntryList where

import Import

getEntryListR :: Handler Html
getEntryListR = do
  user <- requireAuthId
  entries <- runDB $ selectList [EntryOwnerId ==. user] [Asc EntryId]
  defaultLayout $ do
    setTitle "Entries"
    $(widgetFile "entries")

