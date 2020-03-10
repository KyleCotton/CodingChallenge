import qualified Rendering2D
import Game2D
import qualified Rendering3D
import Graphics.UI.GLUT
import Data.IORef
  
main :: IO ()
main = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= (Size 1000 1000)
  getArgsAndInitialize
  w <- createWindow "Game of Life"
  displayCallback $= display
  twoOrThree <- newIORef 0
  keyboardMouseCallback $= Just (keyboardMouse)
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, Size 1000 1000)))
  mainLoop
  
display :: DisplayCallback
display = do
   clear [ ColorBuffer ]
   flush
   
keyboardMouse :: KeyboardMouseCallback
keyboardMouse key Down _ _ = case key of
    (Char '2') -> (Rendering2D.maindraw)
    (Char '3') -> (Rendering3D.maindraw)
    _ -> return()
keyboardMouse _ _ _ _ = return ()
