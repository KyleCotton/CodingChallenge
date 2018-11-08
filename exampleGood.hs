module NewGame where

import Data.Set as Set

type Point = (Int, Int)
type Grid  = Set Point

-- Checks whether a point is alive in the given grid
isAlive :: Grid -> Point -> Bool
isAlive g p = member p g

-- Returns a point's 8 neighbouring points
neighbours :: Point -> Set Point
neighbours p = Set.fromList [
        offset p (dx, dy)
        | dx <- [-1..1], dy <- [-1..1],
        not (dx == 0 && dy == 0)
    ]

-- Offsets a coordinate by given delta
offset :: Point -> (Int, Int) -> Point
offset p d = (fst p + fst d, snd p + snd d)

-- Returns a set of points which includes the living points in the grid and
-- all of thier neighbours
extendGrid :: Grid -> Grid
extendGrid g = union g $ unions [neighbours p | p <- toList g]

-- Returns a set of living neighbours to the given point
livingNeighbours :: Grid -> Point -> Grid
livingNeighbours g p = Set.filter (isAlive g) (neighbours p)

-- Returns the number of living neighbours of the given point
livingNeighbourCount :: Grid -> Point -> Int
livingNeighbourCount g p = Set.size $ livingNeighbours g p

-- Checks whether the given point will be alive in the next generation
lives :: Grid -> Point -> Bool
lives g p
    | isAlive g p = livingNeighbourCount g p `elem` [2, 3]
    | otherwise   = livingNeighbourCount g p `elem` [3]

-- Gives the next generation of the grid
nextGen :: Grid -> Grid
nextGen g = Set.filter (lives g) (extendGrid g)
