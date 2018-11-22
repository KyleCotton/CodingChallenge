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
module Game (Grid, theOtherGrid, nIterations, gridToLivingPoints, gridToDeadPoints) where
import Data.List

type Health   = Bool               -- Each Block has an accociated health
type Location = (Float , Float, Float)    -- Each Block has an cartesian coordinate
type Person   = (Health, Location) -- Each 'Person' will have a health and location
type Grid     = [Person]           -- The grid is represented as a list of all the people

--takes all of the points from aliveStates and converts them into people with those co-ordinates
theOtherGrid :: Grid
theOtherGrid = [ (True,l) | l <- aliveStates ]

--initial living people
aliveStates :: [Location]  -- A list of locations of the alive states
aliveStates = [(50,50,50)
              ,(51,50,50)
              ,(50,51,50)
              ,(51,51,50)]

--takes in a Grid and an integer and returns a list of n Grids each grid the successive generation from the last
nIterations :: Int -> Grid -> [Grid]
--iterates through next Gen called on the grid and uses take to get the first n generations (this uses haskell's lazyness)
nIterations n g = take n $ iterate (nextGen) g

--gridToLivingPoints and fridToDeadPoints get all the livin and all the dead points respectively
gridToLivingPoints, gridToDeadPoints :: Grid -> [Location]
gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]
gridToDeadPoints grd = [coord | (liv, coord) <- grd, not liv]

--nextGen takes a Grid and returns the sucessive generation for the grid 
nextGen :: Grid -> Grid

nextGen gss = {-Then removes all of the dead points from the new grid-}filter (\(l, loc) -> l) {-Then maps isAlive to the new grid to get the next generation-} (map (\p@(h, l) -> (isAlive p gss, l))
                                                    --adds all of the points around the living points (not alread occupied) as dead points to the grid
                                                    ( gss ++
                                                      [ (False,local) |
                                                      local <- nub zombieLocals,
                                                      not (elem local (getPoints gss) ) ] ))
                  where
                    --find all of the points around the all living people in the grid passed in
                    zombieLocals = (concat (map zombieLocal ( gridToLivingPoints gss )))

--takes a Grid and returns a list of the locations of all the peopl in the grid
getPoints :: Grid -> [Location]
getPoints = map (\(p,l) -> l)

--takes a location and returns all of the locations around that location in 3D space
zombieLocal :: Location -> [Location]
zombieLocal (x,y,z) = [ (a,b,c) | a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+1], (a,b,c) /= (x,y,z)]

--This fucntion takes in a person and the current grid and returns whether or not that person will be alive in the next generation
isAlive :: Person -> Grid -> Bool
                             --gets the number of living people around the given person
isAlive (h, (x, y, z)) gss = let gs = length [1 |  (h', (x', y', z'))
                                                   <- gss
                                                         , (x',y',z') /= (x,y,z)
                                                         , x' `elem` [x-1,x,x+1]  --    if the block is alive and 3 or 2 of its neigbours
                                                         , y' `elem` [y-1,y,y+1]  --    are also alive the the block will stay alive.
                                                         , z' `elem` [z-1,z,z+1]] --    if the block is dead and is surrounded by 3 alive
                                in                                                --    neigbours then it will become alive.
                                  case h of
                                    --if the person is alive then if there are 9 people around it it will remain alive otherwise it will die
                                    True  -> (gs == 9)
                                    --if the person is dead then if there are 4 people around it it will 'be born' otherwise it will remain dead
                                    False -> (gs == 4)
