module Import.Premium where

import Import

hasPremium :: User -> IO Bool
hasPremium user = do
  today <- fmap utctDay getCurrentTime
  return $ userPremium user && userPremiumUntil user >= today

