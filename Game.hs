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
module Game (Grid, theGrid, nIterations, gridToLivingPoints) where
import Data.List

type Health   = Bool               -- Each Block has an accociated health
type Location = (Float , Float)    -- Each Block has an cartesian coordinate
type Person   = (Health, Location) -- Each 'Person' will have a health and location
type Grid     = [Person]           -- The grid is represented as a list of all the people

nIterations :: Int -> Grid -> [Grid]             -- This returns a list of grids
nIterations n g = take n $ iterate (nextGen) g   --   after n iterations

gridToLivingPoints :: Grid -> [Location]                     -- This returns a list of locations
gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]  --    of alive points

-- Example starting grid
startPeople, testGrid :: Grid
testGrid = [
  (False,(0,0)),(True,(50,51)),(False,(0,2)),(True,(50,53)),(False,(0,4)),
  (False,(1,0)),(True,(51,51)),(False,(1,2)),(True,(51,53)),(False,(1,4)),
  (False,(2,0)),(True,(52,51)),(False,(2,2)),(True,(52,53)),(False,(2,4)),
  (False,(3,0)),(True,(53,51)),(False,(3,2)),(True,(53,53)),(False,(3,4)),
  (False,(4,0)),(True,(54,51)),(False,(4,2)),(True,(54,53)),(False,(4,4))
  ]

startPeople = [
  (False,(23,25)), (True ,(24,25)), (True ,(25,25)), (True,(26,25)),
  (False,(27,25)), (False,(24,26)), (False,(25,26)), (False,(26,26)),
  (False,(24,24)), (False,(25,24)), (False,(26,24))
  ]

nextGen :: Grid -> Grid                                  -- This maps the next function over
nextGen gss = filter (\steve -> isAlive steve gss) (map (\p@(h, l) -> (isAlive p gss, l))
                                                    ( gss ++
                                                      [ (False,local) |
                                                      local <- nub zombieLocals,
                                                      not (elem local (getPoints gss) ) ] ))
                  where
                    zombieLocals = (concat (map zombieLocal ( gridToLivingPoints gss )))

-- filter (\(p,l) -> isAlive p gss) gss
--

getPoints :: Grid -> [Location]
getPoints = map (\(p,l) -> l)

zombieLocal :: Location -> [Location]
zombieLocal (x,y) = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not ((a==x)&&(b==y)) ]

isAlive :: Person -> Grid -> Bool                                                       -- Function that generates the next grid from the previous
isAlive (h, (x, y)) gss = let gs = length [1 |  (h', (x', y'))                    --    This fucntion takes in a person and the current grid
                                                   <- gss, x' `elem` [x-1,x,x+1]  --    if the block is alive and 3 or 2 of its neigbours
                                                         , y' `elem` [y-1,y,y+1]  --    are also alive the the block will stay alive.
                                                         , h',   (x/=x' || y/=y')]  --    if the block is dead and is surrounded by 3 alive
                                in                                                      --    neigbours then it will become alive.
                                  case h of
                                    True  -> (gs == 2 || gs == 3)
                                    False -> (gs == 3)

-- ### START THE GAME WITH RANDOM GRID
-- THE GRID IS 100 X 100 IN SIZE

-- type Location = (Float , Float)
-- type Person   = (Health, Location)
-- type Grid     = [Person]

theGrid :: Grid
theGrid = [(((fromInteger x, fromInteger y) `elem` aliveStates), (fromInteger x, fromInteger y)) | x <- [0..99], y <- [0..99]]

theOtherGrid :: Grid
theOtherGrid = [ (True,l) | l <- aliveStates ]

aliveStates :: [Location]
aliveStates = [(50,50)
              ,(51,50)
              ,(49,50)
              ,(40,40)
              ,(40,41)
              ,(40,39)]
