module Rendering where

import Data.List hiding (group)
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle, project, rotate)
import Data.IORef
import Control.Concurrent
import Game

myPoints :: [Point]
myPoints = [(0,0,0)]--[(1,1,1),(0,0,0),(-1,-1,-1),(1,1,-1),(-1,1,1),(-5,-3,-4)]

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat, GLfloat)
type Point2D = (GLfloat, GLfloat)
type Angle = GLfloat

gridWidth :: Float
gridWidth = 100

gridHight :: Float
gridHight = 100

--takes a size and a center point and creates a cube at that center point (and returns it's center for 'sorting' later)
makeCube :: Matrix -> GLfloat -> Point -> [Point]
makeCube mat size center = moveCube (rotate mat byOrigin) center
  where
    byOrigin = [(radius', radius', radius'), (-radius', radius', radius'), (-radius', -radius', radius'), (radius', -radius', radius'),
                (radius', radius', -radius'), (-radius', radius', -radius'), (-radius', -radius', -radius'), (radius', -radius', -radius'),
                (radius', radius', radius'), (radius', radius', -radius'), (radius', -radius', -radius'), (radius', -radius', radius'),
                (-radius', radius', radius'), (-radius', radius', -radius'), (-radius', -radius', -radius'), (-radius', -radius', radius'),
                (radius', radius', radius'), (-radius', radius', radius'), (-radius', radius', -radius'), (radius', radius', -radius'),
                (radius', -radius', radius'), (-radius', -radius', radius'), (-radius', -radius', -radius'), (radius', -radius', -radius')]
    radius' = (size/ 2)

--takes a list of points (a cube at the origin) and other point (the cube's center) and addeds the singel point to each point in the list (moves the cube to the point)
moveCube :: [Point] -> Point -> [Point]
moveCube xs (x, y, z)= [((x + x'), (y + y'), (z+z'))| (x', y', z') <- xs]

--takes a list of center points and makes a cube-center pair for each of them
makeCubes :: Matrix -> [Point] -> [[Point]]
makeCubes mat lst = [(makeCube mat 1 point) | point<-lst]

--Takes two matrices and multiplies them
multiplyMat :: Matrix -> Matrix -> Matrix
multiplyMat m1 m2 = if length (transpose m1) == length m2 then [[ sum (zipWith (*) col row) | col <- (transpose m2)] | row <- m1] else error("matrices can't be multiplied")

--Takes a matrix and a vector, multiplies them and returns the resulting vector
multMatVec :: Matrix -> Vector -> Vector
multMatVec mat vec = head (transpose (multiplyMat mat (transpose [vec])))

--Takes in a vector and returns a point made from the first to values in the vector
--points can only have two values so an ordered list is used
--and the rest of the values in the vector are discarded.
vecToPoint :: Vector -> Point
vecToPoint (x:y:z:ns) = (x,y,z)

--Takes a point and converts it to a 2D vector
pointToVec :: Point -> Vector
pointToVec (x,y,z) = [x,y,z]

--Takes in a cube-center pair and rotate both around the origin a given angle using a given rotation matrix 
rotate :: Matrix -> [Point] -> [Point]
rotate  mat  m = [rot s | s <- m]
  where
    rot x = vecToPoint (multMatVec mat (pointToVec x))

--give roation matrix for rotation around the X axis by a given angle
rotationX :: GLfloat -> Matrix
rotationX t = [[1,0,0],
              [0,cos (radians t), -sin (radians t)],
              [0,sin (radians t), cos (radians t)]]

--give roation matrix for rotation around the Y axis by a given angle
rotationY :: GLfloat -> Matrix
rotationY t = [[cos (radians t), 0, -sin (radians t)],
              [0,1,0],
              [sin (radians t), 0, cos (radians t)]]

--give roation matrix for rotation around the Z axis by a given angle
rotationZ :: GLfloat -> Matrix
rotationZ t = [[cos (radians t), -sin (radians t), 0],
              [sin (radians t), cos (radians t),0],
              [0,0,1]]

--takes in an angle in degrees and return the equivalent in radians
radians :: Floating a => a -> a
radians t =  t * 2 * pi / 360

--subtracts one vector from another
subVectors :: Vector -> Vector -> Vector
subVectors v1 v2 = [(v1 !! a) - (v2 !! a) | a <- [0..(length v1 - 1)]]

--projects a list of 3D points into 2D
projects :: GLfloat -> [Point] -> [Point2D]
projects distance points = map (project distance) points

--takes a 3D point and projects it using a projection matrix (created for that point to create the illusing of perspecive)
project :: GLfloat -> Point -> Point2D
project distance point =  vec2DToPoint2D $ multMatVec (proj point) (pointToVec point)
  where
    proj (x, y, z) = [[1/(distance - z),0,0],[0,1/(distance - z),0]]
    vec2DToPoint2D (x:n:ns) = (x,n)

--Gets the distance from a point to the camera (camera is always (0,0,z))
getDist :: GLfloat -> Point -> GLfloat
getDist cam (x,y,z) = sqrt(x*x + y*y + (cam-z)**2)

orderPoints :: GLfloat -> [Point] -> [Point]
orderPoints cam lst = sortBy comparePoints lst
  where
    comparePoints p1 p2 = comp (getDist cam p1) (getDist cam p2)
    comp a b
      | a > b = GT
      | a < b = LT
      | otherwise = EQ

--takes a cube and gets rid of the corner furthest from the camer
--(does this as a form of culling as there will always by one corner (and 3 sides) that can't be seen and so don't need to be drawn)
culling :: GLfloat -> [[Point]] -> [[[Point]]]
culling cam lst = [[face | face <- faces, not ((farPoint.concat $ take 2 faces) `elem` face)]| faces <- world]
  where
    farPoint = maximumBy comparePoints
    comparePoints p1 p2 = comp (getDist cam p1) (getDist cam p2)
    comp a b
      | a > b = GT
      | a < b = LT
      | otherwise = EQ
    world = map (group 6) lst

group :: Int -> [a] -> [[a]]
group _ [] = []
group 6 lst = (take 4 lst):group 5 (drop 4 lst)
group n lst = (take 4 lst):group (n-1) (drop 4 lst)

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  --makes GLUT use double buffering
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= (Size 1000 1000)
  --creates a window
  createWindow "Game Of Life"
  enterGameMode
  reshapeCallback $= Just reshape
  --creates a mutatable variable for the angle of rotation
  angle <- newIORef (0,0,0)
  distance <- newIORef 4
  --displays points
  displayCallback $= (display distance angle)
  --makes changes
  idleCallback $= Just (idle angle)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

exclude :: GLfloat -> Point -> Bool
exclude cam pt = not ((ptToO > (getDist cam (0,0,0))) && (ptToO > (getDist cam pt)))
  where
    ptToO = (getDist 0 pt)

getRotations :: (GLfloat, GLfloat, GLfloat) -> Matrix
getRotations (xt,yt,zt) = multiplyMat (rotationZ zt) (multiplyMat (rotationX xt) (rotationY yt))

--displays the points as a loop
display :: IORef GLfloat -> IORef (GLfloat, GLfloat, GLfloat) -> DisplayCallback
display distance angle = do
  --helper function that creates a color
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  --clears the color buffer
  clear [ ColorBuffer ]
  --gets the value of the mutatable variable and stores it as angle'
  dist <- readIORef distance
  angle' <- readIORef angle
  let mat = getRotations angle'
  --renders groups of four vertexs as squares
  renderPrimitive Quads $ do
    --sets the color to red
    color3f 1 1 1
    --takes a list of points and converts them to cubes, rotates them around the origin, orders them in distance from the camera and projects them into 2D
    -- then takes each new 2D point and draws it
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (concat . concat. (map (map (projects dist))) . (culling dist)  $ ((makeCubes mat). (orderPoints dist) $ filter (\pt -> exclude dist pt ) (rotate mat myPoints)))
  flush
  --limits the frame rate
  threadDelay (1000 `div` 20)
  --tells the double buffer to update
  swapBuffers

--changes the angle of rotation by 0.5 degrees each time it's called
idle :: IORef (GLfloat, GLfloat, GLfloat) -> IdleCallback
idle angle = do
  --gets the value of the mutatable variable and stores it as angle'
  angle' <- readIORef angle
  --sets the balue of the mutatble variable to (angle' + 0.05) mod 360
  writeIORef angle (gtsangs angle')
  postRedisplay Nothing
    where
      newAng angles = angles + 0.05
      --makeshift mod 360 for floating point as i couldn't get `mod` to work
      getNewAng ang = if (newAng ang) > 360 then (newAng ang) - 360 else (newAng ang)
      gtsangs (x, y, z) = (x,getNewAng y, z)



