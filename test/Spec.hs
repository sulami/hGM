import Test.QuickCheck

import Game.DnD.GM.Entry (findMatches)

main = quickCheck (\l s -> all (`elem` l) (findMatches l s))

