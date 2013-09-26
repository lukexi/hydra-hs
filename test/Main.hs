import System.Hardware.Hydra


main = do
  putStrLn "Hello, Hydrato!"
  initSuccess <- sixenseInit
  
  let maxB = maxBases
  putStrLn ("max bases: " ++ (show maxB))
  setBaseSuccess <- setActiveBase 0
  autoEnableHemisphereTracking 0
  setActiveBase 0
  colorSuccess <- setBaseColor 255 100 0
  putStrLn ("Set active base to 0: " ++ show setBaseSuccess)
  mapM_ (\i -> controllerEnabled i >>= \e -> putStrLn ("Controller " ++ (show i) ++ " enabled: " ++ (show e))) [0..3]
  active <- numActiveControllers
  putStrLn ("max controllers: " ++ show getMaxControllers ++ ", active: " ++ show active)
  mapM_ (\i -> baseConnected i >>= \e -> putStrLn ("Base " ++ (show i) ++ " connected: " ++ (show e))) [0..1]


  exitSuccess <- sixenseExit
  putStrLn "Done."