module Handler.HandoutNew where

import           Handler.HandoutEdit (handoutForm)
import           Import
import           Import.Premium      (hasPremium)

getHandoutNewR :: CampaignId -> Handler Html
getHandoutNewR cid = do
  Entity uid user <- requireAuth
  prem <- hasPremium user
  camp <- runDB $ get404 cid
  unless (uid == campaignOwnerId camp) $ redirect HomeR
  if prem
    then do
      (handoutWidget, enctype) <- generateFormPost $ handoutForm cid
      defaultLayout $ do
        setTitle "New Handout"
        $(widgetFile "handoutnew")
    else
      redirect . EntriesR $ EntryListR cid

postHandoutNewR :: CampaignId -> Handler Html
postHandoutNewR cid = do
  _ <- requireAuthId
  ((res,_), _) <- runFormPost $ handoutForm cid
  case res of
    FormSuccess handout -> do
      handoutId <- runDB $ insert handout
      setMessage . toHtml $ handoutName handout <> " created"
      redirect . HandoutsR $ HandoutR handoutId
    _ -> defaultLayout $ do
      setMessage "Error creating handout."
      $(widgetFile "error")

