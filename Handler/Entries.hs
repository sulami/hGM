module Handler.Entries where

import Import

getEntriesR :: Handler Html
getEntriesR = do
  entries <- runDB $ selectList [] [Asc EntryId, LimitTo 5]
  defaultLayout $(widgetFile "entries")

