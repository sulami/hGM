module Entry (
  Entry, newEntry,
  findMatches, findEntries
  ) where

-- | Describes an entry.
data Entry = Entry {
      name :: String,
      content :: String
    }

instance Show Entry where
  show (Entry t "") = show $ Entry t "<empty>"
  show (Entry t c ) = t ++ "\n" ++ c

-- | Create a new entry without content.
newEntry :: String -> Entry
newEntry t = Entry t ""

-- | Check which words of a string are matching words in a list.
findMatches :: [String] -> String -> [String]
findMatches list str = filter (`elem` list) $ words str

-- | Check which words of an entry match entries in a list and return the
-- names, so that the corresponing entries can be looked up.
findEntries :: [Entry] -> Entry -> [String]
findEntries list ent = findMatches (map name list) (content ent)

