module Handler.EntryEdit where

import           Import
import           Import.Semantic (renderSemantic)

import           Data.Char (isAlphaNum)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Text.Markdown as MD

import           Yesod.Text.Markdown

entryForm :: CampaignId -> [(Text, Maybe CategoryId)] -> Form Entry
entryForm camp cats = renderSemantic $ Entry
  <$> areq textField "Title" Nothing
  <*> areq markdownField "Content" Nothing
  <*> pure camp
  <*> areq (selectFieldList $ ("Uncategorized" :: Text, Nothing) : cats)
      "Category" Nothing
  <*> pure []

prepEntryForm :: Entry -> [(Text, Maybe CategoryId)] -> Form Entry
prepEntryForm entry cats = renderSemantic $ Entry
  <$> areq textField "Title" (Just $ entryName entry)
  <*> areq markdownField "Content" (Just $ entryContent entry)
  <*> pure (entryCampaignId entry)
  <*> areq (selectFieldList $ ("Uncategorized" :: Text, Nothing) : cats)
      "Category" Nothing
  <*> pure (entryInThis entry)

getEntryEditR :: EntryId -> Handler Html
getEntryEditR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  let cid = entryCampaignId entry
  camp <- runDB $ get404 cid
  if user /= campaignOwnerId camp
    then do
      setMessage "Permission denied."
      defaultLayout $ $(widgetFile "error")
    else do
      categories <- runDB $ selectList [CategoryCampaignId ==. cid]
                    [Asc CategoryName]
      (entryWidget, enctype) <- generateFormPost . prepEntryForm entry $
                                  formatCategories categories
      defaultLayout $ do
        setTitle . toHtml $ entryName entry
        $(widgetFile "entryedit")

postEntryEditR :: EntryId -> Handler Html
postEntryEditR entryId = do
  user <- requireAuthId
  entry <- runDB $ get404 entryId
  let cid = entryCampaignId entry
  camp <- runDB $ get404 cid
  categories <- runDB $ selectList [CategoryCampaignId ==. cid]
                  [Asc CategoryName]
  ((res,_), _) <- runFormPost . entryForm cid $ formatCategories categories
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
    let oldInThis = entryInThis e
        newInThis = map (\(Entity x _) -> x) $ filter (inEntry e) allEntries
    when (newInThis /= oldInThis) . runDB $ update k [EntryInThis =. newInThis]

-- | Get the Element from an entity, discarding the key.
entityToThing :: Entity a -> a
entityToThing (Entity _ t) = t

-- | Is an entry referenced in this one?
inEntry :: Entry -> Entity Entry -> Bool
inEntry e = (`textMatch` TS.words (unmarkdown $ entryContent e)) . TS.words .
              entryName . entityToThing

-- | Convert Markdown to strict text.
unmarkdown :: MD.Markdown -> Text
unmarkdown = TL.toStrict . (\(MD.Markdown e) -> e)

-- | Match multi-word names. Case-insensitive. Ignores punctuation.
textMatch :: [Text] -> [Text] -> Bool
textMatch snippet content = tm (map prep snippet) (map prep content)
  where
    prep :: Text -> Text
    prep = TS.toCaseFold . TS.filter isAlphaNum

    tm :: [Text] -> [Text] -> Bool
    tm [] _  = False
    tm _  [] = False
    tm s  c  = take (length s) c == s || tm s (drop 1 c)

-- | Format the available categories into (name, id) tuples
formatCategories :: [Entity Category] -> [(Text, Maybe CategoryId)]
formatCategories = map (\(Entity k c) -> (categoryName c, Just k))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

