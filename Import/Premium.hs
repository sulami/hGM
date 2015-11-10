module Import.Premium where

import Import
import Data.Time.Calendar (addDays)

prizes :: [(Int, Float)]
prizes = [ ( 1,  2.99)
         , ( 3,  6.99)
         , ( 6, 12.99)
         , (12, 24.99)
         , (24, 44.99)
         ]

-- | Check if a user has active premium that expires sometime in the future.
hasPremium :: MonadIO m => User -> m Bool
hasPremium user = do
  today <- liftIO $ fmap utctDay getCurrentTime
  return $ userPremiumUntil user >= today

-- | Give a user premium for a set of days.
addPremium :: UserId -> Integer -> Handler ()
addPremium uid days = do
  mbUser <- runDB $ get uid
  case mbUser of
    Nothing   -> return ()
    Just user -> do
      today <- liftIO $ fmap utctDay getCurrentTime
      let old = userPremiumUntil user
      runDB $ update uid [ UserPremiumUntil =. addDays days (max today old) ]

