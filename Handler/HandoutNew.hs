module Handler.HandoutNew where

import Import
import Import.Premium (hasPremium)
import Import.Semantic (renderSemantic)

import Yesod.Text.Markdown

handoutForm :: CampaignId -> Form Handout
handoutForm camp = renderSemantic $ Handout
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> pure camp

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
postHandoutNewR campaignId = error "Not yet implemented: postHandoutNewR"
