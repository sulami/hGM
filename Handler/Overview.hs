module Handler.Overview where

import Import

getOverviewR :: Handler Html
getOverviewR = do
  Entity uid user <- requireAuth
  defaultLayout $ do
    setTitle "Account"
    $(widgetFile "accountoverview")

