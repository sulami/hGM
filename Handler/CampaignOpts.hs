module Handler.CampaignOpts where

import           Import
import           Import.Semantic (renderSemantic)

prepCampForm :: Campaign -> Form Campaign
prepCampForm camp = renderSemantic $ Campaign
  <$> areq textField "Name" (Just $ campaignName camp)
  <*> pure (campaignOwnerId camp)

getCampaignOptsR :: CampaignId -> Handler Html
getCampaignOptsR campaignId = do
  user <- requireAuthId
  campaign <- runDB $ get404 campaignId
  if user /= campaignOwnerId campaign
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      categories <- runDB $ selectList [CategoryCampaignId ==. campaignId]
                      [Asc CategoryName]
      (campWidget, enctype) <- generateFormPost $ prepCampForm campaign
      defaultLayout $ do
        setTitle . toHtml $ campaignName campaign
        $(widgetFile "campaignopts")

postCampaignOptsR :: CampaignId -> Handler Html
postCampaignOptsR campaignId = error "Not yet implemented: postCampaignOptsR"

