module Handler.HandoutEdit where

import Import
import Import.Semantic (renderSemantic)

import Yesod.Text.Markdown

handoutForm :: CampaignId -> Form Handout
handoutForm camp = renderSemantic $ Handout
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> pure camp

prepHandoutForm :: Handout -> Form Handout
prepHandoutForm handout = renderSemantic $ Handout
  <$> areq textField "Title" (Just $ handoutName handout)
  <*> areq markdownField "Content" (Just $ handoutContent handout)
  <*> pure (handoutCampaignId handout)

getHandoutEditR :: HandoutId -> Handler Html
getHandoutEditR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  camp <- runDB . get404 $ handoutCampaignId handout
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      (handoutWidget, enctype) <- generateFormPost $ prepHandoutForm handout
      defaultLayout $ do
        setTitle . toHtml $ handoutName handout
        $(widgetFile "handoutedit")

postHandoutEditR :: HandoutId -> Handler Html
postHandoutEditR handoutId = do
  user <- requireAuthId
  handout <- runDB $ get404 handoutId
  let cid = handoutCampaignId handout
  camp <- runDB $ get404 cid
  ((res,_), _) <- runFormPost $ handoutForm cid
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      case res of
        FormSuccess handoutData -> do
          runDB $ replace handoutId handoutData
          setMessage . toHtml $ handoutName handoutData <> " saved"
          redirect . HandoutsR $ HandoutR handoutId
        _ -> defaultLayout $ do
          setMessage "Permission denied."
          $(widgetFile "error")

