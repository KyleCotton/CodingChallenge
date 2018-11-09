module Game where

import System.Random
import Control.Monad
import Test.QuickCheck

type Health   = Bool
type Location = (Int   , Int)
type Person   = (Health, Location)
type Grid     = [Person]

{-
GAME INSTRUCTIONS:
Alive: 2-3 Friends -> Alive
       else        -> Dead

Dead:  3 Friends   -> Alive
       else        -> Dead
-}

nIterations :: Int -> Grid -> [Grid]
nIterations n g = take n $ iterate (nextGen) g

startPeople, startPeople' :: [Person]
startPeople = [(False,(0,0)),(True,(0,1)),(False,(0,2)),(True,(0,3)),(False,(0,4)), (True, (1,2))]

startPeople' = [(False,(0,0)),(False,(0,1)),(True,(0,2)),(False,(0,3)),(False,(0,4)), (True, (1,2))]

emptyGrid :: Int -> Int -> Grid
emptyGrid n p = [(False, (x,y)) | x <- [0..(n-1)], y <-[0..(p-1)]]

nextGen :: Grid -> Grid
nextGen gs = map (\p@(h, l) -> (isAlive p gs, l)) gs

isAlive :: Person -> Grid -> Bool
isAlive (h, (x, y)) gss = case h of
                            True  -> gs == 2 || gs == 3
                            False -> gs == 3
                          where gs = length [1 | (h', (x', y')) <- gss, x' `elem` [x-1,x,x+1], y' `elem` [y-1,y,y+1], h', (x/=x' || y/=y')] 





-- ### START THE GAME WITH RANDOM GRID
