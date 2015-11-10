module Handler.Purchase where

import Import
import Import.Premium (prizes)
import Secret (stripeSecretKey)

getPurchaseR :: Int -> Handler Html
getPurchaseR months = do
  let p = lookup months prizes
  case p of
    Nothing    -> error "Invalid parameter"
    Just prize -> do
      let sk = stripeSecretKey
      defaultLayout $ do
        setTitle "Get Premium"
        $(widgetFile "purchase")

postPurchaseR :: Int -> Handler Html
postPurchaseR = error "Not yet implemented: postPurchaseR"
