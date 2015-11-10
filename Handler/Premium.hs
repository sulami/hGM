module Handler.Premium where

import Text.Read (read)

import Web.Stripe (stripe)
import Web.Stripe.Charge (Currency (USD), TokenId (..), chargeCardByToken)

import Import
import Import.Premium (prizes)
import Secret

getPremiumR :: Handler Html
getPremiumR = do
  Entity _ user <- requireAuth
  let periods = prizes
      pubKey = stripePublicKey
      userAddr = userIdent user
  defaultLayout $ do
    setTitle "Premium"
    $(widgetFile "premium")

postPremiumR :: Handler Html
postPremiumR = do
  Entity uid user <- requireAuth
  token <- lookupPostParam "stripeToken"
  case token of
    Nothing  -> error "No token supplied"
    Just tok -> do
      amount <- lookupPostParam "amount"
      case amount of
        Nothing  -> error "No amount supplied"
        Just amt -> do
          let secKey = stripeSecretKey
          result <- liftIO $ stripe secKey $
            chargeCardByToken (TokenId tok) USD (read $ unpack amt) Nothing
          case result of
            Left stripeError -> error $ show stripeError
            Right charge     -> do
              setMessage "Success"
              redirect HomeR

-- TODO
-- Verify the amount matches the months
-- Give premium to the user
-- Add a proper success page

-- | Convert a float prize to an in-cents int
stripePrize :: Float -> Int
stripePrize = round . (* 100)

