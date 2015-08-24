{-# LANGUAGE CPP #-}
module System.Hardware.Hydra 
  ( initSixense
  , getHands
  , handButtons
  , recalibrate
  , activeButtons
  -- Reexports
  , setActiveBase
  , baseConnected
  , controllerEnabled
  , numActiveControllers
  , SixenseBase
  , ControllerData(..)
  , Button(..)
  , WhichHand(..)
  , Raw.maxBases
  , Raw.getMaxControllers
  ) where
import qualified System.Hardware.Hydra.Raw as Raw
import System.Hardware.Hydra.Raw (ControllerData(..), Button(..), ButtonBits, WhichHand(..))
import Control.Monad.Trans
import Data.Maybe
import Control.Monad
import Data.IORef

data SixenseBase = SixenseBase
  { sixbConnected :: IORef Bool
  }

initSixense :: MonadIO m => m SixenseBase
initSixense = liftIO $ do
  
#if !defined(darwin_HOST_OS)
  _ <- Raw.sixenseInit
#else
  liftIO . putStrLn 
    $  "Hydra disabled as it crashes when not connected \n"
    ++ "(unless using a hacky threadDelay before initialization) \n"
    ++ "and I can't think of a way to work around it... :P"
#endif
  

  base <- SixenseBase 
    <$> newIORef False

  _ <- checkConnected base

  return base

-- | If the base becomes newly connected,
-- set it as the active base and recalibrate the controllers.
-- It's important to check baseConnected before setActiveBase,
-- as setActiveBase will crash otherwise :()
checkConnected :: SixenseBase -> IO Bool
checkConnected base = do
  #if defined(darwin_HOST_OS)
  return False
  #else
  nowConnected <- Raw.baseConnected 0
  wasConnected <- readIORef (sixbConnected base)
  when (nowConnected && not wasConnected) $ do
    _ <- Raw.setActiveBase 0
    recalibrate
  writeIORef (sixbConnected base) nowConnected
  return nowConnected
  #endif


getHands :: MonadIO m => SixenseBase -> m [ControllerData]
getHands base = liftIO $ do
  
  connected <- checkConnected base
  if connected 
    then do
      leftHand   <- Raw.getNewestData 0
      rightHand  <- Raw.getNewestData 1
      return (catMaybes [leftHand, rightHand])
    else do
      return []

-- Via http://sixense.com/forum/vbulletin/showthread.php?3078-Hydra-calibration-code,
-- apparently recalibration just calls autoEnableHemisphereTracking for each controller.
recalibrate :: MonadIO m => m ()
recalibrate = liftIO $ do
  putStrLn "Recalibrating Hydra"
  _ <- Raw.autoEnableHemisphereTracking 0
  _ <- Raw.autoEnableHemisphereTracking 1
  return ()

handButtons :: ControllerData -> [Button]
handButtons = activeButtons . buttons

activeButtons :: ButtonBits -> [Button]
activeButtons buttonBits = catMaybes
    [ check Raw.buttonBumper    ButtonBumper
    , check Raw.buttonJoystick  ButtonJoystick
    , check Raw.button1         Button1
    , check Raw.button2         Button2
    , check Raw.button3         Button3
    , check Raw.button4         Button4
    , check Raw.buttonStart     ButtonStart
    ]
    where check buttonBit button = if Raw.isButtonDown buttonBit buttonBits then Just button else Nothing

-- Reexports

setActiveBase :: MonadIO m => Int -> m Raw.SixenseSuccess
setActiveBase = liftIO . Raw.setActiveBase

controllerEnabled :: MonadIO m => Int -> m Bool
controllerEnabled = liftIO . Raw.controllerEnabled

numActiveControllers :: MonadIO m => m Int
numActiveControllers = liftIO Raw.numActiveControllers

baseConnected :: MonadIO m => Int -> m Bool
baseConnected = liftIO . Raw.baseConnected

