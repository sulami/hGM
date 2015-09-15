module Handler.EntryEdit where

import           Import
import           Import.Semantic (renderSemantic)

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Text.Markdown as MD

import           Yesod.Text.Markdown

entryForm :: CampaignId -> Form Entry
entryForm camp = renderSemantic $ Entry
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> pure camp
  <*> pure []
  <*> pure []

prepEntryForm :: Entry -> Form Entry
prepEntryForm entry = renderSemantic $ Entry
  <$> areq textField "Title" (Just $ entryName entry)
  <*> areq markdownField "Content" (Just $ entryContent entry)
  <*> (pure $ entryCampaignId entry)
  <*> (pure $ entryInThis entry)
  <*> (pure $ entryThisIn entry)

getEntryEditR :: EntryId -> Handler Html
getEntryEditR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  camp <- runDB . get404 $ entryCampaignId entry
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      (entryWidget, enctype) <- generateFormPost $ prepEntryForm entry
      defaultLayout $ do
        setTitle . toHtml $ entryName entry
        $(widgetFile "entryedit")

postEntryEditR :: EntryId -> Handler Html
postEntryEditR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  let cid = entryCampaignId entry
  camp <- runDB $ get404 cid
  ((res,_), _) <- runFormPost $ entryForm cid
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else
      case res of
        FormSuccess entryData -> do
          runDB $ replace entryId entryData
          saveEntry cid entryId entryData
          setMessage . toHtml $ entryName entryData <> " saved"
          redirect . EntriesR $ EntryR entryId
        _ -> defaultLayout $ do
          setMessage "Permission denied."
          $(widgetFile "error")

saveEntry :: CampaignId -> EntryId -> Entry -> Handler ()
saveEntry cid entryId entry = do
  otherEntries' <- runDB $ selectList [EntryCampaignId ==. cid] []
  let otherEntries = map entityToTuple otherEntries'
      inThis = filter (inEntry entry) otherEntries
      thisIn = filter (entryIn entry) otherEntries
  runDB $ update entryId [ EntryInThis =. map fst inThis
                         , EntryThisIn =. map fst thisIn ]
  forM_ inThis $ \(key, _) -> do
    old <- runDB $ get404 key
    runDB $ update key [ EntryThisIn =. (entryId : entryThisIn old) ]
  forM_ thisIn $ \(key, _) -> do
    old <- runDB $ get404 key
    runDB $ update key [ EntryInThis =. (entryId : entryInThis old) ]

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

