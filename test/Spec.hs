import Test.QuickCheck

import Entry

main = quickCheck (\l s -> all (`elem` l) (findMatches l s))

