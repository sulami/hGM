module Handler.AddCampaign where

import Import

import Handler.EntryEdit (renderSemantic)

campaignForm :: UserId -> Form Campaign
campaignForm user = renderSemantic $ Campaign
  <$> areq textField "Name" Nothing
  <*> pure user

getAddCampaignR :: Handler Html
getAddCampaignR = do
  user <- requireAuthId
  (campaignWidget, enctype) <- generateFormPost $ campaignForm user
  defaultLayout $ do
    setTitle "New Campaign"
    $(widgetFile "addcampaign")

postAddCampaignR :: Handler Html
postAddCampaignR = do
  user <- requireAuthId
  ((res,_), _) <- runFormPost $ campaignForm user
  case res of
    FormSuccess campaignData -> do
      camp <- runDB $ insert campaignData
      setMessage $ toHtml $ campaignName campaignData <> " created"
      redirect $ EntriesR $ EntryListR camp
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")
