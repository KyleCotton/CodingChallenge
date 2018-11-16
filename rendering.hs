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
rotateX :: Angle -> Point -> Point
rotateX t m = vecToPoint (multMatVec (rotationX t) (pointToVec m))

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

makeCubes :: [Point] -> [Point]
makeCubes lst = concat [makeCube (1) point | point<-lst]

subVectors :: Vector -> Vector -> Vector
subVectors v1 v2 = [(v1 !! a) - (v2 !! a) | a <- [0..(length v1 - 1)]]

projects :: GLfloat -> [Point] -> [Point2D]
projects distance points = map (project distance) points

project :: GLfloat -> Point -> Point2D
project distance point =  vec2DToPoint2D $ multMatVec (proj point) (pointToVec point)
  where
    proj (x, y, z) = [[1/(distance - z),0,0],[0,1/(distance - z),0]]
    vec2DToPoint2D (x:n:ns) = (x,n)

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
  angle <- newIORef 0
  --displays points
  displayCallback $= (display angle)
  --makes changes
  idleCallback $= Just (idle angle)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

--displays the points as a loop
display :: IORef GLfloat -> DisplayCallback
display angle = do
  --helper function that creates a color
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  --clears the color buffer
  clear [ ColorBuffer ]
  --gets the value of the mutatable variable and stores it as angle'
  angle' <- readIORef angle
  --renders groups of four vertexs as squares
  renderPrimitive Points $ do
    --sets the color to red
    color3f 1 0 0
    --takes a list of points and converts them to vertexs
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) ((projects 2) $ map (rotateX angle') (makeCubes myPoints))
  flush
  --limits the frame rate to 60 fps
  threadDelay (1000000 `div` 60)
  --tells the double buffer to update
  swapBuffers

--makes changes to variables as needed
idle :: IORef GLfloat -> IdleCallback
idle angle = do
  --gets the value of the mutatable variable and stores it as angle'
  angle' <- readIORef angle
  --sets the balue of the mutatble variabel to (angle' + 0.05) mod 360
  writeIORef angle (getNewAng angle')
  postRedisplay Nothing
    where
      newAng angles = angles + 0.05
      getNewAng ang = if (newAng ang) > 360 then (newAng ang) - 360 else (newAng ang)



