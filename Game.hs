{-
GAME LOGIC MODULE:

This module contains all of the logic of the game controlling the
transition between the stages of the game.


GAME RULES:
Alive: 2-3 Friends -> Alive
       else        -> Dead

Dead:  3 Friends   -> Alive
       else        -> Dead

-}
module Game (Grid, startPeople, nIterations, gridToLivingPoints) where

import System.Random

type Health   = Bool               -- Each Block has an accociated health
type Location = (Float , Float)    -- Each Block has an cartesian coordinate
type Person   = (Health, Location) -- Each 'Person ' will have a health and location
type Grid     = [Person]           -- The grid is represented as a list of all the people

nIterations :: Int -> Grid -> [Grid]             -- This returns a list of grids 
nIterations n g = take n $ iterate (nextGen) g   --     after n iterations

gridToLivingPoints :: Grid -> [Location]               -- This returns a list of locations
gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]  --     of alive points

-- Example starting grid
startPeople, testGrid :: Grid
testGrid = undefined
startPeople = [
  (False,(0,0)),(True,(0,1)),(False,(0,2)),(True,(0,3)),(False,(0,4)),
  (False,(1,0)),(True,(1,1)),(False,(1,2)),(True,(1,3)),(False,(1,4)),
  (False,(2,0)),(True,(2,1)),(False,(2,2)),(True,(2,3)),(False,(2,4)),
  (False,(3,0)),(True,(3,1)),(False,(3,2)),(True,(3,3)),(False,(3,4)),
  (False,(4,0)),(True,(4,1)),(False,(4,2)),(True,(4,3)),(False,(4,4))
  ]


nextGen :: Grid -> Grid                                  -- This maps the next function over
nextGen gss = map (\p@(h, l) -> (isAlive p gss, l)) gss  --     the entire grid of people

isAlive :: Person -> Grid -> Bool                                                       -- Function that generates the next grid from the previous
isAlive (h'', (x'', y'')) gss = let gs = length [1 | (h', (x', y'))                     --    This fucntion takes in a person and the current grid 
                                                   <- gss, x' `elem` [x''-1,x'',x''+1]  --    if the block is alive and 3 or 2 of its neigbours    
                                                         , y' `elem` [y''-1,y'',y''+1]  --    are also alive the the block will stay alive.        
                                                         , h'  , (x''/=x' || y''/=y')]  --    if the block is dead and is surrounded by 3 alive    
                                in                                                      --    neigbours then it will become alive.                 
                                  case h'' of
                                    True  -> (gs == 2 || gs == 3)
                                    False -> (gs == 3)

-- ### START THE GAME WITH RANDOM GRID
randomGrid :: Grid
randomGrid = undefined

