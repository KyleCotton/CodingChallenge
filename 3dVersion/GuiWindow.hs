module GuiWindow
  where

-- import Graphics.UI.GLUT
 
-- gui :: IO ()
-- gui = do
--   (_progName, _args) <- getArgsAndInitialize
--    _window           <- createWindow "GAME OF LIFE!"
--    displayCallback   $= display
--    mainLoop
 
-- display :: DisplayCallback
-- display = do
--   clear [ ColorBuffer ]
--   flush



import Graphics.UI.GLUT as GL
 
myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]
 
main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop
 
display :: DisplayCallback
display = do 
  clear [ColorBuffer]
  renderPrimitive Points $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush

GL.renderPrimitive Points = do
  vertex (Vertex3 ...)
  vertex (Vertex3 ...)  
