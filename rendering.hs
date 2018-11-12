module Rendering where

import Data.List
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle)
import Data.IORef
import Control.Concurrent
import Game

myPoints :: [Point]
myPoints = [(0.5,0.5), (-0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat)
type Angle = GLfloat

matrix1 :: Matrix
matrix1 = [[1,2,3],[3,5,6],[6,8,1]]

makeSquare :: GLfloat -> Point -> [Point]
makeSquare size center = moveSquare byOrigin center
  where
    moveSquare xs (x, y)= [((x + x'), (y + y'))| (x', y') <- xs]
    byOrigin = [(radius', radius'), (-radius', radius'), (-radius', -radius'), (radius', -radius')]
    radius' = (size/ 2)

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
vecToPoint (x:y:ns) = (x,y)

--Takes a point and converts it to a 2D vector
pointToVec :: Point -> Vector
pointToVec (x,y) = [x,y]

--Takes in a vecotr and an anple and returns the vector rotated around the origin by the given angle
rotate2D :: Angle -> Vector -> Vector
rotate2D t m = multMatVec rotation m
  where
    rotation  = [[cos (radians t), (-1) * (sin (radians t))], [sin (radians t), cos (radians t)]]
    radians t =  t * 2 * pi / 360

--Takes an angle and list of points and rotates each of the poins around the origin
rotatePoints :: Angle -> [Point] -> [Point]
rotatePoints theta pts = [ vecToPoint . (rotate2D theta) $ pointToVec pt| pt <- pts]

gridToSquares :: Grid -> [Point]
gridToSquares grd = undefined

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  --makes GLUT use double buffering
  initialDisplayMode $= [DoubleBuffered]
  --creates a window
  createWindow "Game Of Life"
  enterGameMode
  reshapeCallback $= Just reshape
  --creates a mutatable variable for the angle of rotation
  angle <- newIORef 0.0
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
  renderPrimitive Quads $ do
    --sets the color to red
    color3f 1 0 0
    --takes a list of points and converts them to vertexs
    
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (rotatePoints angle' myPoints)
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


