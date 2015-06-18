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
import Linear
import Data.IORef
import Control.Concurrent

data SixenseBase = SixenseBase
  { sixbCorrection :: IORef (M44 Float)
  }

-- initSixense :: MonadIO m => m SixenseBase
initSixense :: MonadIO m => m ()
initSixense = liftIO $ do
  putStrLn "HELLO!"
  _ <- Raw.sixenseInit
  -- threadDelay 2000000

  putStrLn "THERE"
  -- correctionRef <- newIORef identity
  connected <- Raw.baseConnected 0
  when connected $ do
    putStrLn "SAILOR"
    _ <- Raw.setActiveBase 0
    _ <- Raw.autoEnableHemisphereTracking 0
    return ()

  -- return SixenseBase
  --   { sixbCorrection = correctionRef
  --   }

-- getHands :: MonadIO m => SixenseBase -> m [ControllerData]
getHands :: MonadIO m => m [ControllerData]
-- getHands base = liftIO $ do
getHands = liftIO $ do  
  connected <- Raw.baseConnected 0
  if connected 
    then do
      _ <- Raw.setActiveBase 0
      _ <- Raw.autoEnableHemisphereTracking 0
      _ <- Raw.autoEnableHemisphereTracking 1

      leftHand   <- Raw.getNewestData 0
      rightHand  <- Raw.getNewestData 1
      return (catMaybes [leftHand, rightHand])
    else do
      return []

-- Via http://sixense.com/forum/vbulletin/showthread.php?3078-Hydra-calibration-code,
-- apparently recalibration just calls autoEnableHemisphereTracking for each controller.
-- recalibrate :: MonadIO m => SixenseBase -> m ()
-- recalibrate base = liftIO $ do
recalibrate :: MonadIO m => m ()
recalibrate = liftIO $ do
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

