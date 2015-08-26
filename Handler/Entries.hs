module Handler.Entries where

import Import

getEntriesR :: Handler Html
getEntriesR = do
  user <- requireAuthId
  entries <- runDB $ selectList [] [Asc EntryId, LimitTo 5]
  defaultLayout $(widgetFile "entries")

