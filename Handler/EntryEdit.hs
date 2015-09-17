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
  <*> pure (entryCampaignId entry)
  <*> pure (entryInThis entry)
  <*> pure (entryThisIn entry)

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
          updateRelationships cid
          setMessage . toHtml $ entryName entryData <> " saved"
          redirect . EntriesR $ EntryR entryId
        _ -> defaultLayout $ do
          setMessage "Permission denied."
          $(widgetFile "error")

-- | Update all the relationships between entries in a campaign when something
-- has changed. This needs to be called after commiting the change.
updateRelationships :: CampaignId -> Handler ()
updateRelationships cid = do
  allEntries <- runDB $ selectList [EntryCampaignId ==. cid] []
  forM_ allEntries $ \(Entity k e) -> do
        -- Original set of relationships.
    let oldInThis = entryInThis e
        oldThisIn = entryThisIn e
        -- The new set of relationships.
        newInThis = map (\(Entity x _) -> x) $ filter (inEntry e) allEntries
        newThisIn = map (\(Entity x _) -> x) $ filter (entryIn e) allEntries
    when (newInThis /= oldInThis || newThisIn /= oldThisIn) . runDB $
      update k [ EntryInThis =. newInThis, EntryThisIn =. newThisIn ]

entityToThing :: Entity a -> a
entityToThing (Entity _ t) = t

-- | Is an entry referenced in this one?
inEntry :: Entry -> Entity Entry -> Bool
inEntry e = (`textMatch` TS.words (unmarkdown $ entryContent e)) . TS.words .
              entryName . entityToThing

-- | Is this entry referenced in another one?
entryIn :: Entry -> Entity Entry -> Bool
entryIn e = textMatch (TS.words $ entryName e) . TS.words . unmarkdown .
              entryContent . entityToThing

unmarkdown :: MD.Markdown -> Text
unmarkdown = TL.toStrict . (\(MD.Markdown e) -> e)

-- | Match multi-word names. Case-insensitive. Ignores punctuation.
textMatch :: [Text] -> [Text] -> Bool
textMatch [] _  = False
textMatch _  [] = False
textMatch s  c  =
  map (TS.toCaseFold . TS.filter (`elem` (['a'..'z'] ++ ['A'..'Z'])))
    (take (length s) c) == map TS.toCaseFold s || textMatch s (drop 1 c)

