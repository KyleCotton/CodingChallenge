module Rendering where

import Data.List
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle, project)
import Data.IORef
import Control.Concurrent
import Game

myPoints :: [Point]
myPoints = [(0,0,0)]

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat, GLfloat)
type Point2D = (GLfloat, GLfloat)
type Angle = GLfloat

gridWidth :: Float
gridWidth = 100

gridHight :: Float
gridHight = 100

makeCube :: GLfloat -> Point -> [Point]
makeCube size center = moveCube byOrigin center
  where
    byOrigin = [(radius', radius', radius'), (-radius', radius', radius'), (-radius', -radius', radius'), (radius', -radius', radius'),
                (radius', radius', -radius'), (-radius', radius', -radius'), (-radius', -radius', -radius'), (radius', -radius', -radius')]
    radius' = (size/ 2)

moveCube :: [Point] -> Point -> [Point]
moveCube xs (x, y, z)= [((x + x'), (y + y'), (z+z'))| (x', y', z') <- xs]

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

--Takes in a vecotr and an anple and returns the vector rotated around the origin by the given angle
rotateX :: Angle -> Vector -> Vector
rotateX t m = multMatVec (rotationX t) m

rotationX t = [[1,0,0],
              [0,cos (radians t), -sin (radians t)],
              [0,sin (radians t), cos (radians t)]]

cRotationX t = [[1,0,0],
              [0,cos (radians t), sin (radians t)],
              [0,-sin (radians t), cos (radians t)]]

rotateY :: Angle -> Vector -> Vector
rotateY t m = multMatVec (rotationY t) m

rotationY t = [[cos (radians t), 0, -sin (radians t)],
              [0,1,0],
              [sin (radians t), 0, cos (radians t)]]

rotateZ :: Angle -> Vector -> Vector
rotateZ t m = multMatVec (rotationZ t) m

rotationZ t = [[cos (radians t), -sin (radians t), 0],
              [sin (radians t), cos (radians t),0],
              [0,0,1]]

cRotationZ t = [[cos (radians t), sin (radians t), 0],
              [-sin (radians t), cos (radians t),0],
              [0,0,1]]

radians :: Floating a => a -> a
radians t =  t * 2 * pi / 360

--Takes an angle and list of points and rotates each of the poins around the origin
rotatePoints :: Angle -> [Point] -> [Point]
rotatePoints theta pts = [ vecToPoint . (rotateZ theta) $ pointToVec pt| pt <- pts]

--mapPoints :: [Point] -> [Point]
--mapPoints lst = [(((x-xOffset)/xOffset)+(1/gridWidth), -(((y-yOffset)/yOffset) + (1/gridWidth))) | (y,x)<-lst]
 -- where
   -- xOffset = (gridWidth/2)
   -- yOffset = (gridHight/2)

makeCubes :: [Point] -> [Point]
makeCubes lst = concat [makeCube (1) point | point<-lst]

subVectors :: Vector -> Vector -> Vector
subVectors v1 v2 = [(v1 !! a) - (v2 !! a) | a <- [0..(length v1 - 1)]]

projects :: (Angle, Angle, Angle) -> [Point] -> [Point2D]
projects angles points = map (project angles [5,5,5] [0,0,0.1]) (map pointToVec points)

--project :: (Angle, Angle, Angle) -> [Point] -> [Point] -> [Point] -> [Point2D]
--project (ax, ay, az) pointPos camPos =  multMatVec  (multiplyMat (multiplyMat (cRotationX ax) (rotationY ay)) (cRotationZ az)) (subVectors pointPos camPos)

project :: (Angle, Angle, Angle) -> Vector -> Vector -> Vector -> Point2D
project (ax, ay, az) camPos disPos pointPos = proj $ vecToPoint (subVectors pointPos camPos)
  where
    get2DPoints (dx', dy', dz') (ex, ey, ez) = ((ez/dz')*dx' + ex, (ez/dz')*dy' + ey)
    proj points  = get2DPoints (dx points , dy points, dz points) (vecToPoint disPos)
    cx = cos(radians ax)
    cy = cos(radians ay)
    cz = cos(radians az)
    sx = sin(radians ax)
    sy = sin(radians ay)
    sz = sin(radians az)
    dx (x,y,z) = (cy*(sz*y + cz*x) - sy*z)
    dy (x,y,z) = (sx*(cy*z + sy*(sz*y + cz*x)) + cx*(cz*y - sz*x))
    dz (x,y,z) = (cx*(cy*z + sy*(sz*y + cz*x)) - sx*(cz*y - sz*x))

--prop_projects :: (Angle, Angle, Angle) -> Vector -> Vector -> Property
--prop_projects angles pointP camP = length pointP == 3 && length camP == 3 ==> (project angles pointP camP) == (project' angles pointP camP)


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
  cameraAngle <- newIORef (90, 90, 90)
  --displays points
  displayCallback $= (display cameraAngle)
  --makes changes
  idleCallback $= Just (idle cameraAngle)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

--displays the points as a loop
display :: IORef (Angle, Angle, Angle) -> DisplayCallback
display cameraAngle = do
  --helper function that creates a color
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  --clears the color buffer
  clear [ ColorBuffer ]
  --gets the value of the mutatable variable and stores it as angle'
  angle' <- readIORef cameraAngle
  --renders groups of four vertexs as squares
  renderPrimitive Points $ do
    --sets the color to red
    color3f 1 0 0
    --takes a list of points and converts them to vertexs
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) ((projects angle') $ makeCubes myPoints)
  flush
  --limits the frame rate to 60 fps
  threadDelay (1000000 `div` 60)
  --tells the double buffer to update
  swapBuffers

--makes changes to variables as needed
idle :: IORef (Angle, Angle, Angle) -> IdleCallback
idle angle = do
  --gets the value of the mutatable variable and stores it as angle'
  angle' <- readIORef angle
  --sets the balue of the mutatble variabel to (angle' + 0.05) mod 360
  writeIORef angle (getNewAngs angle')
  postRedisplay Nothing
    where
      newAng angles = angles + 0.05
      getNewAng ang = if (newAng ang) > 360 then (newAng ang) - 360 else (newAng ang)
      getNewAngs (a1, a2, a3) = (getNewAng a1, getNewAng a2, getNewAng a3)



