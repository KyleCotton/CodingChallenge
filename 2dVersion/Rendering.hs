--module Rendering where

import Data.List
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle)
import Data.IORef
import Control.Concurrent
import Game

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat)
type Angle = GLfloat

gridWidth :: Float
gridWidth = 100

gridHight :: Float
gridHight = 100

--takes in a size and a point and creates a square of the given size at that point
makeSquare :: GLfloat -> Point -> [Point]
--creates a square of the given size at the origin then moves it to the point
makeSquare size center = moveSquare byOrigin center
  where
    --moves a square by adding the values of the point to each point 
    moveSquare xs (x, y)= [((x + x'), (y + y'))| (x', y') <- xs]
    --creates a square around the origin
    byOrigin = [(radius', radius'), (-radius', radius'), (-radius', -radius'), (radius', -radius')]
    radius' = (size/ 2)

--takes in a list of points with values from 0 to 100 and 'maps' them from -1 to 1
mapPoints :: [Point] -> [Point]
mapPoints lst = [(((x-xOffset)/xOffset)+(1/gridWidth), -(((y-yOffset)/yOffset) + (1/gridWidth))) | (y,x)<-lst]
  where
    xOffset = (gridWidth/2)
    yOffset = (gridHight/2)

--takes a list of points and returns a list of squares (list of four points) for each point
makeSquares :: [Point] -> [Point]
makeSquares lst = concat [makeSquare (2/gridWidth) point | point<-lst]

numOfGens :: Int
numOfGens = 100

--gets all generations that will be used of a given grid
gens :: [Grid]
gens = [x | x <- (nIterations numOfGens theOtherGrid), x /= []]

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
  run <- newIORef True
  --displays points
  displayCallback $= (display gens generation)
  --checks wheter to change generation by looking at user input
  keyboardMouseCallback $= Just (keyboardMouse run)
  --chages to the next generation
  idleCallback $= Just (idle run generation)
  mainLoop

--if the user presses the space bar, change the next generation 'flag' to false
keyboardMouse :: IORef Bool -> KeyboardMouseCallback
--if a key is pressed
keyboardMouse run key Down _ _ =  do
  run' <- readIORef run
  --if that key is the spacebar invert the vlaue of the next generation 'flag' otherwise do nothing
  if key == (Char ' ') then (writeIORef run (not run')) else return ()
--if no key is pressed do nothing
keyboardMouse _ _ _ _ _  = return ()

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
    --sets the colour to green
    color3f 0 1 0
    --gets the current generation and converts it too squares, then draws those squares
    mapM_ (\(x, y) -> vertex $ Vertex2 x y) (makeSquares . mapPoints $ gridToLivingPoints (population !! gen))
  flush
  --limits the frame rate
  threadDelay (200000)
  --tells the double buffer to update
  swapBuffers

--changes the current generation by 1
idle :: IORef Bool -> IORef Int -> IdleCallback
idle run gen = do
  gen' <- readIORef gen
  run' <- readIORef run
  --if the next generation 'flag' is true then move to the next generation otherwise do nothing
  if run' then writeIORef gen (nextGen gen') else return ()
  postRedisplay Nothing
    where
      --moves to the next generation then when it gets to the last generation it goes back to the start
      nextGen curGen = (curGen + 1) `mod` (length gens-1)
