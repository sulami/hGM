-- | This module handles all sorts of dice rolling stuff.

module Game.DnD.GM.Dice where

import Prelude
import System.Random (StdGen, setStdGen, getStdGen, mkStdGen, randomR)

-- | Use an initial integer to deterministically initialize the RNG.
initialize :: Int -> StdGen
initialize = mkStdGen

-- | Use the system-wide RNG to randomly initialize the RNG.
initializeIO :: IO StdGen
initializeIO = getStdGen

-- | Use the system-wide RNG to use any of the pure functions.
rollIO :: (StdGen -> (a, StdGen)) -> IO a
rollIO f = do
  rng <- getStdGen
  let (rv, nrng) = f rng
  setStdGen nrng
  return rv

-- | Use the system-wide RNG to use any of the pure functions with a parameter.
roll1IO :: (StdGen -> b -> a) -> b -> IO a
roll1IO f p0 = do g <- getStdGen
                  return $ f g p0

-- | Use the system-wide RNG to use any of the pure functions with two
-- parameters.
roll2IO :: (StdGen -> c -> b -> a) -> c -> b -> IO a
roll2IO f p0 p1 = do g <- getStdGen
                     return $ f g p0 p1

-- | Roll a single die with d sides.
roll :: StdGen -> Int -> (Int, StdGen)
roll g d = head $ rolls g 1 d

-- | Roll n dice with d sides each and return all the results in a list.
rolls :: StdGen -> Int -> Int -> [(Int, StdGen)]
rolls _ 0 _ = []
rolls g n d = let (rv, ng) = randomR (1, d) g
              in (rv, ng) : rolls ng (n-1) d

-- | Get a single random element from a list.
randFromList :: StdGen -> [a] -> Maybe a
randFromList _ [] = Nothing
randFromList g l  = Just $ head $ randsFromList g 1 l

-- | Get a number of random, unique elements from a list. Can return at most
-- all elements from the list once.
randsFromList :: StdGen -> Int -> [a] -> [a]
randsFromList _ _ [] = []
randsFromList _ 0 _  = []
randsFromList g n l  = let (i, g') = randomR (0, length l - 1) g
                           l' = take i l ++ drop (i+1) l
                       in l !! i : randsFromList g' (n-1) l'

-- | Determine the chance that something happens.
chance :: StdGen -> Int -> (Bool, StdGen)
chance g c = let (r, ng) = roll g 100
              in (r >= c, ng)

-- | Flip a coin.
flipCoin :: StdGen -> (Bool, StdGen)
flipCoin g = chance g 50

-- | Roll a D100.
d100 :: StdGen -> (Int, StdGen)
d100 g = roll g 100

-- | Roll a D20.
d20 :: StdGen -> (Int, StdGen)
d20 g = roll g 20

-- | Roll a D12.
d12 :: StdGen -> (Int, StdGen)
d12 g = roll g 12

-- | Roll a D10.
d10 :: StdGen -> (Int, StdGen)
d10 g = roll g 10

-- | Roll a D8.
d8 :: StdGen -> (Int, StdGen)
d8 g = roll g 8

-- | Roll a D6.
d6 :: StdGen -> (Int, StdGen)
d6 g = roll g 6

-- | Roll a D4.
d4 :: StdGen -> (Int, StdGen)
d4 g = roll g 4

