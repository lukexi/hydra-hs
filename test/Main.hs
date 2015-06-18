import System.Hardware.Hydra
import Control.Monad
import Control.Concurrent

main = do
  forkIO $ forever $ do
    print "hey"
    threadDelay 100000
  putStrLn "Hello, Hydrato!"
  base <- initSixense

  let maxB = maxBases
  putStrLn ("max bases: " ++ (show maxB))
  
  mapM_ (\i -> controllerEnabled i >>= \e -> putStrLn ("Controller " ++ (show i) ++ " enabled: " ++ (show e))) [0..3]
  active <- numActiveControllers
  putStrLn ("max controllers: " ++ show getMaxControllers ++ ", active: " ++ show active)
  mapM_ (\i -> baseConnected i >>= \e -> putStrLn ("Base " ++ (show i) ++ " connected: " ++ (show e))) [0..1]

  replicateM_ 1000 $ do
      -- newest <- getHands base
      newest <- getHands
      print newest
      threadDelay (1000000 `div` 60)
      -- forM_ newest $ \hand -> when (ButtonStart `elem` handButtons hand) $ recalibrate base
      forM_ newest $ \hand -> when (ButtonStart `elem` handButtons hand) $ recalibrate
  -- exitSuccess <- sixenseExit
  putStrLn "Done."
