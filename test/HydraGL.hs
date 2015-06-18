{-# LANGUAGE LambdaCase #-}
-- import Graphics.UI.GLFW.Pal
-- import Graphics.GL.Pal
-- import Graphics.GL
import Linear
import Control.Monad
--import Cube -- This file crashes (on OS X 10.11, ghc 7.10.1) if -framework OpenGL is passed, which will be triggered by importing this
import Data.Bits
import System.Hardware.Hydra

(resX, resY) = (1920, 1080)

main :: IO ()
-- main = withWindow "GLFW Pal" resX resY $ \(win, eventsChan) -> do
main = do
  base <- initSixense

  return ()
  -- cubeProg <- createShaderProgram "test/cube.vert" "test/cube.frag"
  -- cube     <- makeCube cubeProg
  -- useProgram cubeProg

  -- glClearColor 1 1 0 1
  -- glEnable GL_DEPTH_TEST

  -- whileWindow win $ do
  --   processEvents eventsChan (closeOnEscape win)

  --   -- Clear the framebuffer
  --   glClear ( GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT )

  --   -- Render our scene
  --   let projection = perspective 45 (fromIntegral resX/fromIntegral resY) 0.01 1000
  --       model      = mkTransformation 1 (V3 0 0 (-4))
  --       view       = lookAt (V3 0 2 5) (V3 0 0 (-4)) (V3 0 1 0)
  --       mvp        = projection !*! view !*! model
  --       (x,y,w,h)  = (0,0,fromIntegral resX,fromIntegral resY)

  --   glViewport x y w h

  --   renderCube cube mvp

  --   swapBuffers win