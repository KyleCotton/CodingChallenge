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

startPeople :: [Person]
startPeople = [(False,(0,0)),(True,(0,1)),(False,(0,2)),(True,(0,3)),(False,(0,4))]


-- TESTING GRID
emptyGrid :: Int -> Int -> Grid
emptyGrid n p = [(False, (x,y)) | x <- [0..(n-1)], y <-[0..(p-1)]]

-- use traspose to check adjacent column
nextGen :: Grid -> Grid
nextGen ((h,(x,y)):gs) = undefined

isAlive :: Person -> Grid -> Bool
isAlive (h, (x, y)) gss@((h', (x', y')):gs) = undefined





-- ### START THE GAME WITH RANDOM GRID
