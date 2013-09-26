{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- | A wrapper for the Sixense SDK, based on the C SDK.
module System.Hardware.Hydra
       (
         -- * Initialization
         sixenseInit
       , sixenseExit
       , autoEnableHemisphereTracking
         -- * General information
       -- , maxControllers
       , getMaxControllers
       , maxBases
       , setActiveBase
       , baseConnected
       , controllerEnabled
       , numActiveControllers  
       , historySize
         -- * Types
       , SixenseSuccess(..)
       , ControllerID
       , Button
       , buttonBumper, buttonJoystick
       , button1, button2, button3, button4
       , buttonStart
       , ControllerData(..)
         -- * Obtaining data
       , getData
       , getAllData
       , getNewestData
       , getAllNewestData
         -- * Miscellaneous
       , setBaseColor
       )
  where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Storable
import qualified Data.Packed.Vector as Vector
import Data.Packed.Vector(Vector)
import qualified Data.Packed.Matrix as Matrix
import Data.Packed.Matrix(Matrix,(><))
import Control.Monad
import Control.Applicative
import Control.Concurrent(threadDelay)

#include <sixense.h>

newtype Button = Button { unButton :: CUInt }
               deriving (Eq,Show)
                               
#{enum Button, Button
 , buttonBumper = SIXENSE_BUTTON_BUMPER
 , buttonJoystick = SIXENSE_BUTTON_JOYSTICK
 , button1 = SIXENSE_BUTTON_1
 , button2 = SIXENSE_BUTTON_2
 , button3 = SIXENSE_BUTTON_3
 , button4 = SIXENSE_BUTTON_4
 , buttonStart = SIXENSE_BUTTON_START
 }
  
combineButtons :: [Button] -> Button
combineButtons = Button . foldr ((.|.) . unButton) 0

-- | Returns the maximum number of controllers supported by the Sixense control system.
maxControllers :: Int
maxControllers = (fromIntegral :: CInt -> Int) #const SIXENSE_MAX_CONTROLLERS

data SixenseSuccess = Success | Failure deriving (Show, Eq)

fromCInt :: CInt -> SixenseSuccess
fromCInt i = if i == -1 then Failure else Success

mFromCInt :: IO CInt -> IO SixenseSuccess
mFromCInt = liftM fromCInt

data ControllerData = ControllerData 
                      { pos :: Vector Float
                      , rotMat :: Matrix Float
                      , joystickX :: !Float
                      , joystickY :: !Float
                      , trigger :: !Float
                      , buttons :: !Button
                      , sequenceNumber :: !Word8
                      , rotQuat :: Vector Float
                      , firmwareRevision :: !CUShort
                      , hardwareRevision :: !CUShort
                      , packetType :: !CUShort
                      , magneticFrequency :: !CUShort
                      , enabled :: !Bool
                      , controllerIndex :: !CInt
                      , isDocked :: !Bool
                      , whichHand :: !Word8
                      , hemiTrackingEnabled :: !Bool
                      }
                      
instance Storable ControllerData where
  sizeOf _ = #{size sixenseControllerData}
  alignment _ = alignment (undefined :: CInt) -- alignment should be alignment of largest data type in the C struct (we could also use CFloat here instead)
  peek p = ControllerData 
           <$> (do
                   ptr <- (#{peek sixenseControllerData, pos} p) :: IO (Ptr CFloat)
                   lst <- peekArray 3 ptr
                   return $ Vector.fromList $ map realToFrac lst)
           <*> (do
                   ptr <- (#{peek sixenseControllerData, rot_mat} p) :: IO (Ptr CFloat)
                   lst <- peekArray (3 * 3) ptr
                   return $ (3><3) $ map realToFrac lst)
           <*> liftM realToFrac ((#{peek sixenseControllerData, joystick_x } p) :: IO CFloat)
           <*> liftM realToFrac ((#{peek sixenseControllerData, joystick_y } p) :: IO CFloat)
           <*> liftM realToFrac ((#{peek sixenseControllerData, trigger } p) :: IO CFloat)
           <*> liftM Button (#{peek sixenseControllerData, buttons } p)
           <*> liftM fromIntegral ((#{peek sixenseControllerData, sequence_number } p) :: IO CUChar)
           <*> (do
                   ptr <- (#{peek sixenseControllerData, rot_quat} p) :: IO (Ptr CFloat)
                   lst <- peekArray 4 ptr
                   return $ Vector.fromList $ map realToFrac lst)
           <*> (#{peek sixenseControllerData, firmware_revision } p)
           <*> (#{peek sixenseControllerData, hardware_revision } p)
           <*> (#{peek sixenseControllerData, packet_type } p)
           <*> (#{peek sixenseControllerData, magnetic_frequency } p)
           <*> liftM (/= 0) ((#{peek sixenseControllerData, enabled } p) :: IO CInt)
           <*> (#{peek sixenseControllerData, controller_index } p)
           <*> liftM (/= 0) ((#{peek sixenseControllerData, is_docked } p) :: IO CUChar) -- ??
           <*> liftM fromIntegral ((#{peek sixenseControllerData, which_hand } p) :: IO CUChar)
           <*> liftM (/= 0) ((#{peek sixenseControllerData, hemi_tracking_enabled } p) :: IO CUChar)
  poke p x = do
    (pokeArray 
     (#{ptr sixenseControllerData, pos} p) 
     (Vector.toList $ pos x))
    (pokeArray 
     (#{ptr sixenseControllerData, rot_mat} p) 
     (Vector.toList $ Matrix.flatten $ rotMat x))
    #{poke sixenseControllerData, joystick_x} p (joystickX x)
    #{poke sixenseControllerData, joystick_y} p (joystickY x)
    #{poke sixenseControllerData, trigger} p (trigger x)
    #{poke sixenseControllerData, buttons} p (unButton $ buttons x)
    #{poke sixenseControllerData, sequence_number} p (sequenceNumber x)
    (pokeArray 
     (#{ptr sixenseControllerData, rot_quat} p) 
     (Vector.toList $ rotQuat x))
    #{poke sixenseControllerData, firmware_revision} p (firmwareRevision x)
    #{poke sixenseControllerData, hardware_revision} p (hardwareRevision x)
    #{poke sixenseControllerData, packet_type} p (packetType x)
    #{poke sixenseControllerData, magnetic_frequency} p (magneticFrequency x)
    #{poke sixenseControllerData, enabled} p (((fromIntegral . fromEnum) :: Bool -> CInt) $ enabled x)
    #{poke sixenseControllerData, controller_index} p (controllerIndex x)
    #{poke sixenseControllerData, is_docked} p (((fromIntegral . fromEnum) :: Bool -> CInt) $ isDocked x)
    #{poke sixenseControllerData, which_hand} p (whichHand x)
    #{poke sixenseControllerData, hemi_tracking_enabled} p (((fromIntegral . fromEnum) :: Bool -> CInt) $ hemiTrackingEnabled x)
    
                             
data AllControllerData = AllControllerData { controllers :: [ControllerData] }
                                
type ControllerID = Int

foreign import ccall "sixense.h sixenseInit"
  c_sixsenseInit :: IO CInt
                   
-- | Initialize the Sixense library.
-- This function initializes the Sixense library. It must be called at least one time per application. Subsequent calls will have no effect. Once initialized, the other Sixense function calls will work as described until sixenseExit() is called.
sixenseInit :: IO SixenseSuccess
sixenseInit = do 
  r <- mFromCInt c_sixsenseInit
  -- delay for 2 seconds. 
  -- It takes some unknown amount of time before things are actually ready
  threadDelay 2000000
  return r

foreign import ccall "sixense.h sixenseExit"
  c_sixsenseExit :: IO CInt
                   
-- | Shut down the Sixense library.
-- This shuts down the Sixense library. After this function call, all Sixense API calls will return failure, until sixenseInit() is called again.
sixenseExit :: IO SixenseSuccess
sixenseExit = mFromCInt c_sixsenseExit


foreign import ccall "sixense.h sixenseAutoEnableHemisphereTracking"
  c_sixenseAutoEnableHemisphereTracking :: CInt -> IO CInt
                   
-- | Shut down the Sixense library.
-- This shuts down the Sixense library. After this function call, all Sixense API calls will return failure, until sixenseInit() is called again.
autoEnableHemisphereTracking :: ControllerID -> IO SixenseSuccess
autoEnableHemisphereTracking controller = mFromCInt $ c_sixenseAutoEnableHemisphereTracking (fromIntegral controller)
{-
SIXENSE_EXPORT int sixenseSetHemisphereTrackingMode( int which_controller, int state );
SIXENSE_EXPORT int sixenseGetHemisphereTrackingMode( int which_controller, int *state );

SIXENSE_EXPORT int sixenseAutoEnableHemisphereTracking( int which_controller );
-}



foreign import ccall "sixense.h sixenseGetMaxBases"
  c_sixenseGetMaxBases :: CInt
                         
maxBases :: Int
maxBases = fromIntegral c_sixenseGetMaxBases

foreign import ccall "sixense.h sixenseSetActiveBase"
  c_sixenseSetActiveBase :: CInt -> IO CInt
                         
setActiveBase :: Int -> IO SixenseSuccess
setActiveBase = mFromCInt . c_sixenseSetActiveBase . fromIntegral

foreign import ccall "sixense.h sixenseIsBaseConnected"
  c_sixenseIsBaseConnected :: CInt -> IO CInt
                         
baseConnected :: ControllerID -> IO Bool
baseConnected = liftM (/= 0) . c_sixenseIsBaseConnected . fromIntegral



foreign import ccall "sixense.h sixenseGetMaxControllers"
  c_sixenseGetMaxControllers :: CInt
                         
getMaxControllers :: Int
getMaxControllers = fromIntegral c_sixenseGetMaxControllers

foreign import ccall "sixense.h sixenseIsControllerEnabled"
  c_sixenseIsControllerEnabled :: CInt -> IO CInt
                         
-- | Returns true if the referenced controller is currently connected to the Control Unit.
-- This call is used to determine whether or not a given controller is powered on and connected to the system. The argument is an index between 0 and the maximum number of supported controllers.
controllerEnabled :: ControllerID -> IO Bool
controllerEnabled = (liftM (/= 0)) . c_sixenseIsControllerEnabled . fromIntegral

foreign import ccall "sixense.h sixenseGetNumActiveControllers"
  c_sixenseGetNumActiveControllers :: IO CInt
                                     
numActiveControllers :: IO Int
numActiveControllers = (liftM fromIntegral) c_sixenseGetNumActiveControllers

foreign import ccall "sixense.h sixenseGetHistorySize"
  c_sixenseGetHistorySize :: IO CInt
                                     
historySize :: IO Int
historySize = (liftM fromIntegral) c_sixenseGetHistorySize


foreign import ccall "sixense.h sixenseGetData"
  c_sixenseGetData :: CInt -> CInt -> Ptr ControllerData -> IO CInt

-- historyLength :: 0-9
-- | Get state of one of the controllers, selecting how far back into a history of the last 10 updates. 
getData :: ControllerID -- ^ The ID of the desired controller. Valid values are from 0 to 3. If the desired controller is not connected, an empty data packet is returned. Empty data packets are initialized to a zero position and the identity rotation matrix.
          -> Int -- ^ length of the history to obtain. 0-9
          -> IO (Maybe ControllerData)
getData which historyLength = alloca $ \dataPtr -> do
    success <- mFromCInt (c_sixenseGetData (fromIntegral which) (fromIntegral historyLength) dataPtr)
    case success of 
      Success -> peek dataPtr >>= return . Just 
      Failure -> return Nothing
      
      
foreign import ccall "sixense.h sixenseGetAllData"
  c_sixenseGetAllData :: CInt -> Ptr ControllerData -> IO CInt
                     
-- | Get state of all of the controllers, selecting how far back into a history of the last 10 updates. 
getAllData :: Int -- ^ length of the history to obtain. 0-9
           -> IO (Maybe [ControllerData])
getAllData historyLength = allocaArray maxControllers $ \dataPtr -> do
    success <- mFromCInt (c_sixenseGetAllData (fromIntegral historyLength) dataPtr)
    case success of 
      Success -> peekArray maxControllers dataPtr >>= return . Just 
      Failure -> return Nothing

      
foreign import ccall "sixense.h sixenseGetNewestData"
  c_sixenseGetNewestData :: CInt -> Ptr ControllerData -> IO CInt

-- | Get the most recent state of one of the controllers.
getNewestData :: ControllerID -> IO (Maybe ControllerData)
getNewestData which = alloca $ \dataPtr -> do
    success <- mFromCInt (c_sixenseGetNewestData (fromIntegral which) dataPtr)
    case success of 
      Success -> peek dataPtr >>= return . Just 
      Failure -> return Nothing


foreign import ccall "sixense.h sixenseGetNewestData"
  c_sixenseGetAllNewestData :: Ptr ControllerData -> IO CInt
                           
-- | Get the most recent state of all of the controllers.
getAllNewestData :: IO (Maybe [ControllerData])
getAllNewestData = allocaArray maxControllers $ \dataPtr -> do
    success <- mFromCInt (c_sixenseGetAllNewestData dataPtr)
    case success of 
      Success -> peekArray maxControllers dataPtr >>= return . Just 
      Failure -> return Nothing


-- TODO: Only implement the functions you actually need so as to not sit on this forever.

{-

SIXENSE_EXPORT int sixenseSetHighPriorityBindingEnabled( int on_or_off );
SIXENSE_EXPORT int sixenseGetHighPriorityBindingEnabled( int *on_or_off );

SIXENSE_EXPORT int sixenseTriggerVibration( int controller_id, int duration_100ms, int pattern_id );

SIXENSE_EXPORT int sixenseSetFilterEnabled( int on_or_off );
SIXENSE_EXPORT int sixenseGetFilterEnabled( int *on_or_off );

SIXENSE_EXPORT int sixenseSetFilterParams( float near_range, float near_val, float far_range, float far_val );
SIXENSE_EXPORT int sixenseGetFilterParams( float *near_range, float *near_val, float *far_range, float *far_val );
-}

foreign import ccall "sixense.h sixenseSetBaseColor"
  c_sixenseSetBaseColor :: CUChar -> CUChar -> CUChar -> IO CInt
                          
setBaseColor :: Int -> Int -> Int -> IO SixenseSuccess
setBaseColor r g b = mFromCInt $ c_sixenseSetBaseColor (fromIntegral r) (fromIntegral g) (fromIntegral b)


-- SIXENSE_EXPORT int sixenseGetBaseColor( unsigned char *red, unsigned char *green, unsigned char *blue );
-- -}