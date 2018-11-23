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
module Game2D (Grid, theOtherGrid, nIterations, gridToLivingPoints, gridToDeadPoints) where
import Data.List
import System.Random
import System.IO.Unsafe

type Health   = Bool               -- Each Block has an accociated health
type Location = (Float , Float)    -- Each Block has an cartesian coordinate
type Person   = (Health, Location) -- Each 'Person' will have a health and location
type Grid     = [Person]           -- The grid is represented as a list of all the people

theOtherGrid :: Grid
theOtherGrid = [ (True,l) | l <- genStates']

--creates a gird of living people
genStates' :: [Location]
genStates' = [(x,y) | x <- [25..75], y <- [25,75]]

--abandoned attempt to make the grid random, worked in testing, broke when compiled, unable to find why
genStates :: Integer -> [Location]
genStates 0 = []
genStates n = (x, y):(genStates (n-1))
  where
    x = get $ getStdRandom(randomR (25,75))
    y = get $ getStdRandom(randomR (25,75))
    get :: IO a -> a
    get = (unsafePerformIO)

floorPoints :: [Location] -> [Location]
floorPoints lst = map (\(x,y) -> (fromIntegral (floor x), fromIntegral(floor y))) lst

aliveStates :: [Location]  -- A list of locations of the alive states
aliveStates = nub . floorPoints $ genStates length
  where length = (unsafePerformIO $ (getStdRandom(randomR (50, 2500))))

--gets n generations of the game of life from the start grid
nIterations :: Int -> Grid -> [Grid]             -- This returns a list of grids
nIterations n g = take n $ iterate (nextGen) g   --   after n iterations

--takes a grid of people and return a list of the locations of livin or dead people respectively
gridToLivingPoints, gridToDeadPoints :: Grid -> [Location]
gridToLivingPoints grd = [coord | (liv, coord) <- grd, liv]
gridToDeadPoints grd = [coord | (liv, coord) <- grd, not liv]

--takes in a grid and returns the grid that will occur next
nextGen :: Grid -> Grid
nextGen gss = filter (\steve -> isAlive steve gss) (map (\p@(h, l) -> (isAlive p gss, l))
                                                    ( gss ++
                                                      [ (False,local) |
                                                      local <- nub zombieLocals,
                                                      not (elem local (getPoints gss) ) ] ))
                  where
                    zombieLocals = (concat (map zombieLocal ( gridToLivingPoints gss )))

--takes a grid and returns all of the points of the peopl in it
getPoints :: Grid -> [Location]
getPoints = map (\(p,l) -> l)

--takes a point and returns a list af the surrounding points
zombieLocal :: Location -> [Location]
zombieLocal (x,y) = [ (a,b) | a <- [x-1..x+1], b <- [y-1..y+1], not ((a==x)&&(b==y)) ]

--takes a person and a grid and returns whether or not the person will be alive in the next generation
isAlive :: Person -> Grid -> Bool                                                       -- Function that generates the next grid from the previous
isAlive (h, (x, y)) gss = let gs = length [1 |  (h', (x', y'))                    --    This fucntion takes in a person and the current grid
                                                   <- gss, x' `elem` [x-1,x,x+1]  --    if the block is alive and 3 or 2 of its neigbours
                                                         , y' `elem` [y-1,y,y+1]  --    are also alive the the block will stay alive.
                                                         , h',   (x/=x' || y/=y')]  --    if the block is dead and is surrounded by 3 alive
                                in                                                      --    neigbours then it will become alive.
                                  case h of
                                    True  -> (gs == 2 || gs == 3)
                                    False -> (gs == 3)
