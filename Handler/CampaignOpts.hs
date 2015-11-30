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
postCampaignOptsR campaignId = do
  user <- requireAuthId
  campaign <- runDB $ get404 campaignId
  ((res,_), _) <- runFormPost $ prepCampForm campaign
  if user /= campaignOwnerId campaign
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      case res of
        FormSuccess campData -> do
          runDB $ replace campaignId campData
          setMessage . toHtml $ campaignName campData <> " saved."
          redirect . EntriesR $ EntryListR campaignId
        _ -> defaultLayout $ do
          setMessage "An error occured."
          $(widgetFile "error")

