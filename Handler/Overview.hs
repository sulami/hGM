module Handler.Overview where

import           Data.Time.Calendar (showGregorian)
import           Import
import           Import.Premium     (hasPremium)

getOverviewR :: Handler Html
getOverviewR = do
  Entity _ user <- requireAuth
  prem <- hasPremium user
  let premiumUntil = showGregorian $ userPremiumUntil user
  defaultLayout $ do
    setTitle "Account"
    $(widgetFile "accountoverview")

