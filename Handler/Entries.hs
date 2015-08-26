module Handler.Entries where

import Import

getEntriesR :: Handler Html
getEntriesR = do
  user <- requireAuthId
  entries <- runDB $ selectList [EntryOwnerId ==. user] [Asc EntryId]
  defaultLayout $(widgetFile "entries")

