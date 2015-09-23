module Handler.Account where

import Import

getAccountR :: Handler Html
getAccountR = do
  Entity uid user <- requireAuth
  defaultLayout $ do
    setTitle "Account"
    $(widgetFile "account")

