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
      -- TODO add these two above to the entry before saving it
      -- TODO modify the other affected entries as well
      -- TODO add this to entryEdit
      entryId <- runDB $ insert entry
      setMessage . toHtml $ entryName entry <> " created"
      redirect . EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

unmarkdown :: MD.Markdown -> Text
unmarkdown = TL.toStrict . (\(MD.Markdown e) -> e)

-- Is an entry referenced in this one?
inEntry :: Entry -> Entry -> Bool
inEntry e = (`textMatch` TS.words (unmarkdown $ entryContent e)) . TS.words .
              entryName

-- Is this entry referenced in another one?
entryIn :: Entry -> Entry -> Bool
entryIn e = textMatch (TS.words $ entryName e) . TS.words . unmarkdown .
              entryContent

-- Match multi-word names.
textMatch :: [Text] -> [Text] -> Bool
textMatch [] _  = False
textMatch _  [] = False
textMatch s  c  = take (length s) c == s || textMatch s (drop 1 c)

