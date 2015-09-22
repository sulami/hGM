module Handler.CampaignNew where

import           Import
import           Import.Semantic (renderSemantic)

campaignForm :: UserId -> Form Campaign
campaignForm user = renderSemantic $ Campaign
  <$> areq textField "Name" Nothing
  <*> pure user

getCampaignNewR :: Handler Html
getCampaignNewR = do
  user <- requireAuthId
  -- TODO see if the account is premium
  ownedCamps <- runDB $ count [CampaignOwnerId ==. user]
  when (ownedCamps >= 1) $ do
    setMessage "Campaign limit reached. Upgrade to Premium for unlimited campaigns."
    redirect HomeR
  (campaignWidget, enctype) <- generateFormPost $ campaignForm user
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

