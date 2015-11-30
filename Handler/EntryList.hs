module Handler.EntryList where

import           Import
import           Import.Premium (hasPremium)

getEntryListR :: CampaignId -> Handler Html
getEntryListR cid = do
  Entity _ user <- requireAuth
  camp <- runDB $ get404 cid
  entries <- runDB $ selectList [EntryCampaignId ==. cid] [Asc EntryName]
  categories <- runDB $ selectList [CategoryCampaignId ==. cid]
                 [Asc CategoryName]
  let groups = sortIntoCategories entries categories
  handouts <- runDB $ selectList [HandoutCampaignId ==. cid] [Asc HandoutName]
  prem <- hasPremium user
  defaultLayout $ do
    setTitle . toHtml $ campaignName camp
    $(widgetFile "entrylist")

-- | Sort entries into categories and return category names coupled with entry
-- entities
sortIntoCategories :: [Entity Entry] -> [Entity Category]
                   -> [(Text, [Entity Entry])]
sortIntoCategories ents cats = categorized ents cats ++ [uncategorized ents]
  where
    uncategorized :: [Entity Entry] -> (Text, [Entity Entry])
    uncategorized es = ("Uncategorized" :: Text, filter isUncategorized es)

    isUncategorized :: Entity Entry -> Bool
    isUncategorized = isNothing . entryCategoryId . extract

    categorized :: [Entity Entry] -> [Entity Category]
                -> [(Text, [Entity Entry])]
    categorized es = map (\(Entity k c) ->
      (categoryName c, entriesInCategory k es))

    entriesInCategory :: CategoryId -> [Entity Entry] -> [Entity Entry]
    entriesInCategory cid = filter (isInCategory cid)

    isInCategory :: CategoryId -> Entity Entry -> Bool
    isInCategory cid ent = Just cid == entryCategoryId (extract ent)

    extract :: Entity a -> a
    extract (Entity _ e) = e

