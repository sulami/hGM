module Handler.CampaignNew where

import           Import
import           Import.Premium  (hasPremium)
import           Import.Semantic (renderSemantic)

campaignForm :: UserId -> Form Campaign
campaignForm user = renderSemantic $ Campaign
  <$> areq textField "Name" Nothing
  <*> pure user

getCampaignNewR :: Handler Html
getCampaignNewR = do
  Entity uid user <- requireAuth
  prem <- hasPremium user
  unless prem $ do
    ownedCamps <- runDB $ count [CampaignOwnerId ==. uid]
    when (ownedCamps >= 1) $ do
      setMessage
        "Campaign limit reached. Upgrade to Premium for unlimited campaigns."
      redirect HomeR
  (campaignWidget, enctype) <- generateFormPost $ campaignForm uid
  defaultLayout $ do
    setTitle "New Campaign"
    $(widgetFile "campaignnew")

postCampaignNewR :: Handler Html
postCampaignNewR = do
  user <- requireAuthId
  ((res,_), _) <- runFormPost $ campaignForm user
  case res of
    FormSuccess campaignData -> do
      camp <- runDB $ insert campaignData
      setMessage . toHtml $ campaignName campaignData <> " created"
      redirect . EntriesR $ EntryListR camp
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

