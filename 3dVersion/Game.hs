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

theGrid :: Grid
theGrid = [( ((fromInteger x, fromInteger y, fromInteger z) `elem` aliveStates)            -- Define the starting Grid for the game
           , (fromInteger x, fromInteger y, fromInteger z) ) | x <- [0..99], y <- [0..99], z <- [0..99]] --     The grid will be all false except
                                                                            --     the ones in aliveStates
theOtherGrid :: Grid
theOtherGrid = [ (True,l) | l <- aliveStates ]

tOG :: Grid
tOG = [ (cond (x,y,z), (fromInteger x, fromInteger y, fromInteger z)) | x <- [0..60], y <- [0..60], z <- [0..60], cond (x,y,z) ]
        where
          cond (a,b,c) = ((mod a 7 == 0) && (mod b 7 == 0) && (mod c 7 ==0) || ((mod a 11 == 0) && (mod b 11 == 0) && (mod c 11 == 0)))

aliveStates :: [Location]  -- A list of locations of the alive states
aliveStates = [(50,50,50)
              ,(51,50,50)
              ,(50,51,50)
              ,(51,51,50)]


nIterations :: Int -> Grid -> [Grid]             -- This returns a list of grids
nIterations n g = take n $ iterate (nextGen) g   --   after n iterations

gridToLivingPoints, gridToDeadPoints :: Grid -> [Location]
gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]
gridToDeadPoints grd = [coord | (liv, coord) <- grd, not liv]

nextGen :: Grid -> Grid
nextGen gss = filter (\(l, loc) -> l) (map (\p@(h, l) -> (isAlive p gss, l))
                                                    ( gss ++
                                                      [ (False,local) |
                                                      local <- nub zombieLocals,
                                                      not (elem local (getPoints gss) ) ] ))
                  where
                    zombieLocals = (concat (map zombieLocal ( gridToLivingPoints gss )))

getPoints :: Grid -> [Location]
getPoints = map (\(p,l) -> l)

zombieLocal :: Location -> [Location]
--zombieLocal (x,y) = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not ((a==x)&&(b==y)) ]
zombieLocal (x,y,z) = [ (a,b,c) | a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+1], (a,b,c) /= (x,y,z)]

isAlive :: Person -> Grid -> Bool                                                       -- Function that generates the next grid from the previous
isAlive (h, (x, y, z)) gss = let gs = length [1 |  (h', (x', y', z'))                    --    This fucntion takes in a person and the current grid
                                                   <- gss
                                                         , (x',y',z') /= (x,y,z)
                                                         , x' `elem` [x-1,x,x+1]  --    if the block is alive and 3 or 2 of its neigbours
                                                         , y' `elem` [y-1,y,y+1]  --    are also alive the the block will stay alive.
                                                         , z' `elem` [z-1,z,z+1]] --    if the block is dead and is surrounded by 3 alive
                                in                                                --    neigbours then it will become alive.
                                  case h of
                                    True  -> (gs == 9)
                                    False -> (gs == 4)
