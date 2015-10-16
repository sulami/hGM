module Handler.Overview where

import Import
import Import.Premium
import Data.Time.Calendar (showGregorian)

getOverviewR :: Handler Html
getOverviewR = do
  Entity uid user <- requireAuth
  prem <- liftIO $ hasPremium user
  let premiumUntil = showGregorian $ userPremiumUntil user
  defaultLayout $ do
    setTitle "Account"
    $(widgetFile "accountoverview")

