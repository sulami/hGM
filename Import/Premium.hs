module Import.Premium where

import Import
import Data.Maybe (fromJust)
import Data.Time.Calendar (addDays)

-- | Check if a user has active premium that expires sometime in the future.
hasPremium :: User -> IO Bool
hasPremium user = do
  today <- fmap utctDay getCurrentTime
  return $ userPremium user && userPremiumUntil user >= today

-- | Give a user premium for a set of days.
addPremium :: UserId -> Integer -> Handler ()
addPremium uid days = do
  user <- fmap fromJust . runDB $ get uid
  prem <- liftIO $ hasPremium user
  today <- liftIO $ fmap utctDay getCurrentTime
  let old = userPremiumUntil user
  if prem
    then runDB $ update uid [ UserPremiumUntil =. addDays days old ]
    else runDB $ update uid [ UserPremium =. True,
                              UserPremiumUntil =. addDays days today ]

