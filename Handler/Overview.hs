module Handler.Overview where

import Import
import Data.Time.Calendar (showGregorian)

getOverviewR :: Handler Html
getOverviewR = do
  Entity uid user <- requireAuth
  let premiumUntil = showGregorian $ userPremiumUntil user
  defaultLayout $ do
    setTitle "Account"
    $(widgetFile "accountoverview")

