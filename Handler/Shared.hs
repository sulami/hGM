module Handler.Shared where

import Import
import Database.Persist.Sql (toSqlKey)

getSharedR :: Int64 -> Handler Html
getSharedR key =
  if key `mod` 23 /= 0
    then notFound
    else do
      let hid = toSqlKey $ key `div` 23
      handout <- runDB $ get404 hid
      defaultLayout $ do
        setTitle . toHtml $ handoutName handout
        $(widgetFile "shared")

