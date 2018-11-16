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
module Game (Grid, theGrid, nIterations, gridToLivingPoints, gridToDeadPoints) where
import Data.List

type Health   = Bool               -- Each Block has an accociated health
type Location = (Float , Float)    -- Each Block has an cartesian coordinate
type Person   = (Health, Location) -- Each 'Person' will have a health and location
type Grid     = [Person]           -- The grid is represented as a list of all the people

theGrid :: Grid
theGrid = [( ((fromInteger x, fromInteger y) `elem` aliveStates)            -- Define the starting Grid for the game
           , (fromInteger x, fromInteger y) ) | x <- [0..99], y <- [0..99]] --     The grid will be all false except
                                                                            --     the ones in aliveStates
theOtherGrid :: Grid
theOtherGrid = [ (True,l) | l <- aliveStates ]

aliveStates :: [Location]  -- A list of locations of the alive states
aliveStates = [(50,50)
              ,(51,50)
              ,(49,50)
              ,(40,40)
              ,(40,41)
              ,(40,39)]


nIterations :: Int -> Grid -> [Grid]             -- This returns a list of grids
nIterations n g = take n $ iterate (nextGen) g   --   after n iterations

gridToLivingPoints, gridToDeadPoints :: Grid -> [Location]                     -- This returns a list of locations

gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]  --    of alive points
gridToDeadPoints grd = [coord | (liv, coord) <- grd, not liv]  --    of alive points

nextGen :: Grid -> Grid                                  -- This maps the next function over
nextGen gss = filter (\steve -> isAlive steve gss) (map (\p@(h, l) -> (isAlive p gss, l))
                                                    ( gss ++
                                                      [ (False,local) |
                                                      local <- nub zombieLocals,
                                                      not (elem local (getPoints gss) ) ] ))
                  where
                    zombieLocals = (concat (map zombieLocal ( gridToLivingPoints gss )))

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
