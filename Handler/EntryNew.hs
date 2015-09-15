module Handler.EntryNew where

import           Import

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Text.Markdown as MD

import           Handler.EntryEdit (entryForm)

getEntryNewR :: CampaignId -> Handler Html
getEntryNewR cid = do
  _ <- requireAuthId
  camp <- runDB $ get404 cid
  (entryWidget, enctype) <- generateFormPost $ entryForm cid
  defaultLayout $(widgetFile "entrynew")

postEntryNewR :: CampaignId -> Handler Html
postEntryNewR cid = do
  _ <- requireAuthId
  ((res,_), _) <- runFormPost $ entryForm cid
  case res of
    FormSuccess entry -> do
      otherEntries' <- runDB $ selectList [EntryCampaignId ==. cid] []
      let otherEntries = map (\(Entity _ e) -> e) otherEntries'
          inThis = filter (inEntry entry) otherEntries
          thisIn = filter (entryIn entry) otherEntries
      entryId <- runDB $ insert entry
      setMessage . toHtml $ entryName entry <> " created"
      redirect . EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

unmarkdown :: MD.Markdown -> Text
unmarkdown = TL.toStrict . (\(MD.Markdown e) -> e)

inEntry :: Entry -> Entry -> Bool
inEntry e = (`elem` TS.words (unmarkdown $ entryContent e)) . entryName

entryIn :: Entry -> Entry -> Bool
entryIn e = elem (entryName e) . TS.words . unmarkdown . entryContent

