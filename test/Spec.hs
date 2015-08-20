import Test.QuickCheck

import Game.DnD.GM

main = quickCheck (\l s -> all (`elem` l) (findMatches l s))

