import System.Hardware.Hydra
import Control.Monad (replicateM_)

main = do
  putStrLn "Hello, Hydrato!"
  initSuccess <- sixenseInit

  let maxB = maxBases
  putStrLn ("max bases: " ++ (show maxB))
  setBaseSuccess <- setActiveBase 0
  autoEnableHemisphereTracking 0
  setActiveBase 0
  putStrLn ("Set active base to 0: " ++ show setBaseSuccess)
  mapM_ (\i -> controllerEnabled i >>= \e -> putStrLn ("Controller " ++ (show i) ++ " enabled: " ++ (show e))) [0..3]
  active <- numActiveControllers
  putStrLn ("max controllers: " ++ show getMaxControllers ++ ", active: " ++ show active)
  mapM_ (\i -> baseConnected i >>= \e -> putStrLn ("Base " ++ (show i) ++ " connected: " ++ (show e))) [0..1]

  replicateM_ 1000 $ do
      newest <- getNewestData 0
      print newest
      newest <- getNewestData 1
      print newest
  exitSuccess <- sixenseExit
  putStrLn "Done."
