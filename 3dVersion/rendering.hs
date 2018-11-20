module Rendering where

import Data.List hiding (group)
import Test.QuickCheck
import Graphics.UI.GLUT hiding (Matrix, Angle, project, rotate)
import Data.IORef
import Data.Fixed
import Control.Concurrent
import Game

myPoints :: [Point]
myPoints = [(1,1,1),(0,0,0),(-1,-1,-1),(1,1,-1),(-1,1,1),(-5,-3,-4)]

type Matrix = [[GLfloat]]
type Vector = [GLfloat]
type Point = (GLfloat, GLfloat, GLfloat)
type Point2D = (GLfloat, GLfloat)
type Angle = GLfloat

--takes a size, a center point and a rotation matrix
--it creates a cube at that center point (as a list of its faces), rotates it and then moves it to the center point
makeCube :: Matrix -> GLfloat -> Point -> [Point]
makeCube mat size center = movePoints (rotate mat byOrigin) center
  where
    byOrigin = [(radius', radius', radius'), (-radius', radius', radius'), (-radius', -radius', radius'), (radius', -radius', radius'),
                (radius', radius', -radius'), (-radius', radius', -radius'), (-radius', -radius', -radius'), (radius', -radius', -radius'),
                (radius', -radius', -radius'), (radius', -radius', radius'), (radius', radius', radius'), (radius', radius', -radius'), 
                (-radius', radius', radius'), (-radius', radius', -radius'), (-radius', -radius', -radius'), (-radius', -radius', radius'),
                (-radius', radius', -radius'), (radius', radius', -radius'),(radius', radius', radius'), (-radius', radius', radius'), 
                (radius', -radius', radius'), (-radius', -radius', radius'), (-radius', -radius', -radius'), (radius', -radius', -radius')]
    radius' = (size/ 2)

--takes a list of points (a cube at the origin, or list of centers) and other point (the cube's center or movement offset) and addeds the singel point to each point in the list (moves the cube to the point, moves the centers)
movePoints :: [Point] -> Point -> [Point]
movePoints xs (x, y, z)= [((x + x'), (y + y'), (z+z'))| (x', y', z') <- xs]

--takes a list of center points and a rotation matrix makes cubes for all of the centers
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

--Takes in a rotation matrix and a list of points and rotates all of the points using the matrix
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
projects :: GLfloat -> [(GLfloat, [[Point]])] -> [(GLfloat, [[Point2D]])]
projects distance pts = map proj pts
  where
    proj (colour, points) = (colour, map (map (project distance)) points)

--takes a 3D point and projects it using a projection matrix (created for that point to create the illusing of perspecive)
project :: GLfloat -> Point -> Point2D
project distance point = project' point (0,0,distance) (0,0,0) (0,0,1)
{-project distance point =  vec2DToPoint2D $ multMatVec (proj point) (pointToVec point)
  where
    proj (x, y, z) = [[1/(distance - z),0,0],[0,1/(distance - z),0]]
    vec2DToPoint2D (x:n:ns) = (x,n)-}

project' :: Point -> Point -> Point -> Point  -> Point2D
project' (ax, ay, az) (cx, cy, cz) (tx, ty, tz) (ex, ey, ez) = (bx, by)
  where
    x = ax - cx
    y = ay - cy
    z = az - cz
    ctx = cos (radians tx)
    cty = cos (radians ty)
    ctz = cos (radians tz)
    stx = sin (radians tx)
    sty = sin (radians ty)
    stz = sin (radians tz)
    dx = cty*(stz*y+ctz*x)-sty*z
    dy = stx*(cty*z+sty*(stz*y+ctz*x))+ctx*(ctz*y-stz*x)
    dz = ctx*(cty*z+sty*(stz*y+ctz*x))+stx*(ctz*y-stz*x)
    bx = (ez/dz)*dx+ex
    by = (ez/dz)*dy+ey

--Gets the distance from a point to the camera (camera is always (0,0,z))
getDist :: GLfloat -> Point -> GLfloat
getDist cam (x,y,z) = sqrt(x*x + y*y + (cam-z)**2)

--takes in a list of center points and sorts them in order of distance from the camera
orderPoints :: GLfloat -> [Point] -> [Point]
orderPoints cam lst = reverse (sortBy (comparePoints cam)lst)

--takes in two points and returns their ordering
comparePoints :: GLfloat ->Point -> Point -> Ordering
comparePoints cam p1 p2 = comp (getDist cam p1) (getDist cam p2)
  where
    comp a b
      | a > b = GT
      | a < b = LT
      | otherwise = EQ

--takes a cube and gets rid of the corner furthest from the camer
--(does this as a form of culling as there will always by one corner (and 3 sides) that can't be seen and so don't need to be drawn)
culling :: GLfloat -> [[Point]] -> [[[Point]]]
culling cam lst = [[face | face <- faces, not ((farPoint.concat $ take 2 faces) `elem` face)]| faces <- world]
  where
    farPoint = maximumBy (comparePoints cam)
    world = map (group 6) lst

--takes in a list of points and groups them in to groups of 4 points
--this is the list of points of the cubes and it groups them into the faces 
group :: Int -> [a] -> [[a]]
group _ [] = []
group 6 lst = (take 4 lst):group 5 (drop 4 lst)
group n lst = (take 4 lst):group (n-1) (drop 4 lst)

--takes a point and the position of the camera (the camera is always at (0,0,_)) and checks if the point is behind the camera
exclude :: GLfloat -> Point -> Bool
exclude cam pt = not ((ptToO > (getDist cam (0,0,0))) && (ptToO > (getDist cam pt)))
  where
    ptToO = (getDist 0 pt)

--takes a tripple of angles and returns the rotation matrix for rotating around the x and y axis (for each angle respectively)
--limited to x and y as those are the only two useful axis for roation (here) this also reduces the amout of
--matrix work needed, slightly speeding up the rendering
getRotations :: (GLfloat, GLfloat) -> Matrix
getRotations (xt,yt) = (multiplyMat (rotationX xt) (rotationY yt))

--Takes a HSB (Hue Saturation Brightness) value for Hue and converts it to and r g b colour for glut to use
hsbToColour :: GLfloat -> IO ()
hsbToColour h = (\(r,g,b) -> color3f r g b) $  getPrimes h
  where
    x = (1 - abs(((h/60) `mod'` 2 - 1)))
    getPrimes h
      | (h < 60) = (1,x,0)
      | (h >= 60 && h < 120) = (x,1,0)
      | (h >= 120 && h < 180) = (0,1,x)
      | (h >= 180 && h < 240) = (0,x,1)
      | (h >= 240 && h < 300) = (x,0,1)
      | otherwise = (1,0,x)

--takes in vlaues for r g and b and returns an colour object glut can use
color3f r g b = color $ Color3 r g (b :: GLfloat)

--takes a list of squares and a list of their centers (in the same order) returns a list of each square paired with its colour value determined by
--its distance from the origin and an offset
squareColour :: [Point] ->  GLfloat -> [[[Point]]] -> [(GLfloat, [[Point]])]
squareColour cents offset lst = [(val (cents !! s), lst !! s) | s <- [0..(length cents - 1)] ]
  where
    val :: Point -> GLfloat
    val ns = (mod' ((+ offset) . (10 *) $ (getDist 0 ns)) 360)

--takes a list of 2D points and returns the vertexs for them
--specifically takes the points for a cube
drawCube :: [[Point2D]] -> IO ()
drawCube lst = mapM_ (\(x, y) -> vertex $ Vertex2 x y)  (concat lst)

--takes a cube colour pair and returns the IO comands to draw that square in the correct colour
getIO :: (GLfloat, [[Point2D]]) -> IO ()
getIO (col, pts) = do hsbToColour col
                      drawCube pts

--determines the number of generations that will be generated
numOfGens :: Int
numOfGens = 55

--gets all generations that will be used of a given grid
gens :: [Grid]
gens = [x | x <- (nIterations numOfGens theOtherGrid), x /= []]

gridWidth :: Float
gridWidth = 100

gridHight :: Float
gridHight = 100

gridDepth :: Float
gridDepth = 100

--takes a list of the locations of living squares (based around 50,50) and moves them to the center of the redering
--area (0,0)
mapPoints :: [Point] -> [Point]
mapPoints lst = [((x-xOffset), -(y-yOffset), (z-zOffset) ) | (x,y,z)<-lst]
  where
    xOffset = (gridWidth/2)
    yOffset = (gridHight/2)
    zOffset = (gridDepth/2)

--main
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  --makes GLUT use double buffering
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= (Size 1000 1000)
  --creates a window
  createWindow "Game Of Life"
  enterGameMode
  reshapeCallback $= Just (reshape (Size 1000 1000))
  --creates a mutatable variable for the angle of rotation
  angle <- newIORef (0,0)
  distance <- newIORef 4
  colour <- newIORef 255
  pos <- newIORef (0,0,0)
  mat <- newIORef (getRotations (0,0))
  generation <- newIORef 0
  --displays points
  displayCallback $= (display generation mat colour distance pos)
  keyboardMouseCallback $= Just (keyboardMouse generation mat distance angle pos)
  --makes changes
  idleCallback $= Just (idle colour)
  mainLoop

reshape :: Size ->  ReshapeCallback
reshape newsize size = do
  viewport $= (Position 0 0, newsize)
  postRedisplay Nothing

--displays the points as a loop
display :: IORef Int -> IORef Matrix -> IORef GLfloat -> IORef GLfloat  -> IORef (GLfloat, GLfloat, GLfloat) -> DisplayCallback
display gen mat' colour distance  pos = do
  --helper function that creates a color
  --clears the color buffer
  clear [ ColorBuffer ]
  --gets the value of the mutatable variable and stores it as angle'
  dist <- readIORef distance
  colour' <- readIORef colour
  pos' <- readIORef pos
  gen' <- readIORef gen
  mat <- readIORef mat'
  --'prepares' the list of centers by rotating, moving ordering then removing as necsasarry 
  let centers = cens gen' mat dist colour' pos'
  --renders groups of four vertexs as squares
  renderPrimitive Quads $ do
    --takes a list of prepare centers  makes cubes at each of theses points
    --then removes the faces of the cube you won't be able to see (most of, it's not perfect)
    --then projects the points to 2D using a persepective projection matrix
    --then converts the points the vertexs and colours
    mapM_ (getIO) ((projects dist) . (squareColour centers colour') . (culling dist) $ (makeCubes mat centers))
  flush
  --tells the double buffer to update
  swapBuffers
    where
      --takes a list of points and rotates them to where they will be for the 'scene'
      --then removes points that will be behind the camera
      --then orders the points in distance to the camera
      cens gen' mat dist colour' pos'= (orderPoints dist) . (filter (\pt -> exclude dist pt)) $ movePoints (rotate mat (mapPoints $ gridToLivingPoints (gens !! gen'))) pos'

keyboardMouse ::  IORef Int -> IORef Matrix -> IORef GLfloat -> IORef (GLfloat, GLfloat) -> IORef (GLfloat, GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse gen mat dist angles pos key Down _ _ = case key of
  (Char ' ') -> gen $~! (nextGen)
  (Char 'w') -> do
               (angles $~! (rotX (2)))
               angles' <- readIORef angles
               (writeIORef mat (getRotations  angles'))
  (Char 's') -> do
               (angles $~! (rotX (-2)))
               angles' <- readIORef angles
               (writeIORef mat (getRotations  angles'))
  (Char 'd') -> do
               (angles $~! (rotY (2)))
               angles' <- readIORef angles
               (writeIORef mat (getRotations  angles'))
  (Char 'a') -> do
               (angles $~! (rotY (-2)))
               angles' <- readIORef angles
               (writeIORef mat (getRotations  angles'))
  (SpecialKey KeyUp   ) -> dist $~! (+ (-0.1))
  (SpecialKey KeyDown ) -> dist $~! (+ 0.1)
  (SpecialKey KeyLeft ) -> pos $~! \(x,y,z) -> (x+0.1,y,z)
  (SpecialKey KeyRight) -> pos $~! \(x,y,z) -> (x-0.1,y,z)
  _ -> return ()
  where
      newVal inc col = col + inc `mod'` 360
      rotY inc (x, y) = (x,newVal inc y)
      rotX inc (x, y) = (newVal inc x,y)
      --moves to the next generation then when it gets to the last generation it goes back to the start
      nextGen curGen = (curGen + 1) `mod` (length gens-1)
keyboardMouse _ _ _ _ _ _ _ _ _ = return ()

--changes the angle of rotation by 0.5 degrees each time it's called
idle :: IORef GLfloat -> IdleCallback
idle colour  = do
  --gets the value of the mutatable variable colour 
  colour' <- readIORef colour
  --writes the new value of the mutatable variable colour
  writeIORef colour (newVal colour')
  postRedisplay Nothing
    where
      newVal col = col + 0.05 `mod'` 360




