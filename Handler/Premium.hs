module Handler.Premium where

import Text.Read (readMaybe)

import Web.Stripe (stripe)
import Web.Stripe.Charge (Currency (USD), TokenId (..), chargeCardByToken)

import Import
import Import.Premium (prizes, addPremium)
import Secret (stripePublicKey, stripeSecretKey)

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
  Entity uid _ <- requireAuth
  token <- lookupPostParam "stripeToken"
  case token of
    Nothing  -> error "No token supplied"
    Just tok -> do
      time <- parseParam "time"
      case time of
        Nothing -> error "Error passing the desired time"
        Just tm -> do
          let prize = lookup tm prizes
          case prize of
            Nothing -> error "Desired time is not available"
            Just pz -> purchasePremium uid (TokenId tok) tm (stripePrize pz)

-- | Convert a float prize to an in-cents int
stripePrize :: Float -> Int
stripePrize = round . (* 100)

-- | Attempt to read a POST parameter
parseParam :: Read a => Text -> Handler (Maybe a)
parseParam name = do
  val <- lookupPostParam name
  case val of
    Nothing -> return Nothing
    Just v  -> return . readMaybe $ unpack v

-- | Carry out the actual Premium purchase
purchasePremium :: UserId -> TokenId -> Int -> Int -> Handler Html
purchasePremium uid token time amount = do
  let secKey = stripeSecretKey
  result <- liftIO $ stripe secKey $
    chargeCardByToken token USD amount Nothing
  case result of
    Left stripeError -> error $ show stripeError
    Right _          -> do
      addPremium uid (toInteger $ time * 30)
      setMessage . toHtml $ "Success! " ++ months time ++ " of Premium purchased"
      redirect $ AccountR OverviewR

-- | Do the damn 1 month/2 months conversion
months :: Int -> String
months 1 = "1 month"
months n = show n ++ " months"

