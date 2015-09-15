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
      let otherEntries = map entityToTuple otherEntries'
          inThis = filter (inEntry entry) otherEntries
          thisIn = filter (entryIn entry) otherEntries
      -- TODO modify the other affected entries as well
      -- TODO add this to entryEdit
      -- FIXME make this a single DB query
      entryId <- runDB $ insert entry
      runDB $ update entryId [ EntryInThis =. map fst inThis
                             , EntryThisIn =. map fst thisIn ]
      setMessage . toHtml $ entryName entry <> " created"
      redirect . EntriesR $ EntryR entryId
    _ -> defaultLayout $ do
      setMessage "Error creating entry."
      $(widgetFile "error")

entityToTuple :: Entity a -> (Key a, a)
entityToTuple (Entity k e) = (k, e)

unmarkdown :: MD.Markdown -> Text
unmarkdown = TL.toStrict . (\(MD.Markdown e) -> e)

-- Is an entry referenced in this one?
inEntry :: Entry -> (Key Entry, Entry) -> Bool
inEntry e = (`textMatch` TS.words (unmarkdown $ entryContent e)) . TS.words .
              entryName . snd

-- Is this entry referenced in another one?
entryIn :: Entry -> (Key Entry, Entry) -> Bool
entryIn e = textMatch (TS.words $ entryName e) . TS.words . unmarkdown .
              entryContent . snd

-- Match multi-word names.
textMatch :: [Text] -> [Text] -> Bool
textMatch [] _  = False
textMatch _  [] = False
textMatch s  c  = take (length s) c == s || textMatch s (drop 1 c)

