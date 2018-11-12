{-
GAME LOGIC MODULE:

GAME INSTRUCTIONS:
Alive: 2-3 Friends -> Alive
       else        -> Dead

Dead:  3 Friends   -> Alive
       else        -> Dead

-}
module Game where

import System.Random
import Control.Monad
import Test.QuickCheck

type Health   = Bool
type Location = (Int   , Int)
type Person   = (Health, Location)
type Grid     = [Person]

-- DON'T THINK THIS FUNCTION IS REQUIRED
-- ### Not Working
-- WRITE STRIP FUNCTION TO REMOVE ALL OF THE FALSE FROM THE GRID
--strip :: [Grid] -> [Grid]
--strip gs = map ( filter (\(h, l) -> h) ) gs

nIterations :: Int -> Grid -> [Grid]
nIterations n g = take n $ iterate (nextGen) g

-- Example starting grid
startPeople :: Grid
startPeople = [
  (False,(0,0)),(True,(0,1)),(False,(0,2)),(True,(0,3)),(False,(0,4)),
  (False,(1,0)),(True,(1,1)),(False,(1,2)),(True,(1,3)),(False,(1,4)),
  (False,(2,0)),(True,(2,1)),(False,(2,2)),(True,(2,3)),(False,(2,4)),
  (False,(3,0)),(True,(3,1)),(False,(3,2)),(True,(3,3)),(False,(3,4)),
  (False,(4,0)),(True,(4,1)),(False,(4,2)),(True,(4,3)),(False,(4,4))
  ]

-- Function that generates the next grid from the previous
nextGen :: Grid -> Grid
nextGen gss = map (\p@(h, l) -> (isAlive p gss, l)) gss

isAlive :: Person -> Grid -> Bool
isAlive (h'', (x'', y'')) gss = let gs = length [1 | (h', (x', y'))
                                                   <- gss, x' `elem` [x''-1,x'',x''+1]
                                                         , y' `elem` [y''-1,y'',y''+1]
                                                         , h'  , (x''/=x' || y''/=y')]
                                in
                                  case h'' of
                                    True  -> (gs == 2 || gs == 3)
                                    False -> (gs == 3)

-- ### START THE GAME WITH RANDOM GRID
