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

gridWidth :: Float
gridWidth = 50

gridHight :: Float
gridHight = 50

makeSquare :: GLfloat -> Point -> [Point]
makeSquare size center = moveSquare byOrigin center
  where
    moveSquare xs (x, y)= [((x + x'), (y + y'))| (x', y') <- xs]
    byOrigin = [(radius', radius'), (-radius', radius'), (-radius', -radius'), (radius', -radius')]
    radius' = (size/ 2)

mapPoints :: [Point] -> [Point]
mapPoints lst = [(((x-xOffset)/xOffset)+(1/gridWidth), -(((y-yOffset)/yOffset) + (1/gridWidth))) | (y,x)<-lst]
  where
    xOffset = (gridWidth/2)
    yOffset = (gridHight/2)

makeSquares :: [Point] -> [Point]
makeSquares lst = concat [makeSquare (2/gridWidth) point | point<-lst]

gens :: [Grid]
gens = nIterations 10 startPeople

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
  generation <- newIORef 0
  --displays points
  displayCallback $= (display gens generation)
  --makes changes
  idleCallback $= Just (idle generation)
  mainLoop

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

--displays the points as a loop
display :: [Grid] -> IORef Int -> DisplayCallback
display population generation  = do
  --helper function that creates a color
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
  --clears the color buffer
  clear [ ColorBuffer ]
  gen <- readIORef generation
  --renders groups of four vertexs as squares
  renderPrimitive Quads $ do
    --sets the color to red
    color3f 1 0 0
    --takes a list of points and converts them to vertexs
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (makeSquares . mapPoints $ gridToLivingPoints (population !! gen))
  flush
  --limits the frame rate to 60 fps
  threadDelay (1000000)
  --tells the double buffer to update
  swapBuffers

--makes changes to variables as needed
idle :: IORef Int -> IdleCallback
idle gen = do
  gen' <- readIORef gen
  writeIORef gen (gen' + 1)
  postRedisplay Nothing


nGrid :: Int -> Grid -> Grid
nGrid n g = (nIterations n g)!!(n-1)

