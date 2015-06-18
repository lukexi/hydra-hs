import System.Hardware.Hydra
import Control.Monad (replicateM_)
import Control.Concurrent

main = do
  putStrLn "Hello, Hydrato!"
  base <- initSixense

  let maxB = maxBases
  putStrLn ("max bases: " ++ (show maxB))
  -- setBaseSuccess <- setActiveBase 0
  -- autoEnableHemisphereTracking 0
  -- setActiveBase 0
  -- putStrLn ("Set active base to 0: " ++ show setBaseSuccess)
  mapM_ (\i -> controllerEnabled i >>= \e -> putStrLn ("Controller " ++ (show i) ++ " enabled: " ++ (show e))) [0..3]
  active <- numActiveControllers
  putStrLn ("max controllers: " ++ show getMaxControllers ++ ", active: " ++ show active)
  mapM_ (\i -> baseConnected i >>= \e -> putStrLn ("Base " ++ (show i) ++ " connected: " ++ (show e))) [0..1]

  replicateM_ 1000 $ do
      newest <- getHands base
      print newest
      threadDelay (1000000 `div` 60)
  -- exitSuccess <- sixenseExit
  putStrLn "Done."
