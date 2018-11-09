module Rendering where

import Data.List
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle)
import Data.IORef

myPoints :: [Point]
myPoints = [(0.5,0.5), (-0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat)
type Angle = GLfloat

matrix1 :: Matrix
matrix1 = [[1,2,3],[3,5,6],[6,8,1]]

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

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  reshapeCallback $= Just reshape
  angle <- newIORef 0.0
  displayCallback $= (display angle)
  idleCallback $= Just (idle angle)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

--displays the points as a loop
display :: IORef GLfloat -> DisplayCallback
display angle = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  clear [ ColorBuffer ]
  angle' <- readIORef angle
  renderPrimitive Quads $ do
    color3f 1 0 0 
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (rotatePoints angle' myPoints)
  flush
  swapBuffers

idle :: IORef GLfloat -> IdleCallback
idle angle = do
  --angle $~! (+ 0.1)
  angle' <- readIORef angle
  writeIORef angle (getNewAng angle')
  postRedisplay Nothing
    where
      newAng angles = angles + 0.05
      getNewAng ang = if (newAng ang) > 360 then (newAng ang) - 360 else (newAng ang)


