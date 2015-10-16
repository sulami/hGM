module Import.Premium where

import Import
import Data.Maybe (fromJust)
import Data.Time.Calendar (addDays)

-- | Check if a user has active premium that expires sometime in the future.
hasPremium :: MonadIO m => User -> m Bool
hasPremium user = do
  today <- liftIO $ fmap utctDay getCurrentTime
  return $ userPremiumUntil user >= today

-- | Give a user premium for a set of days.
addPremium :: UserId -> Integer -> Handler ()
addPremium uid days = do
  user <- fmap fromJust . runDB $ get uid
  today <- liftIO $ fmap utctDay getCurrentTime
  let old = userPremiumUntil user
  runDB $ update uid [ UserPremiumUntil =. addDays days (max today old) ]

