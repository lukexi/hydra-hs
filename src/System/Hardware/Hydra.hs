{-# LINE 1 "Hydra.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Hydra.hsc" #-}

module System.Hardware.Hydra
       ( SixenseSuccess(..)
       , sixenseInit
       , sixenseExit
       -- , maxControllers
       , getMaxControllers
       , maxBases
       -- , setActiveBase
       -- , baseConnected
       , ControllerID
       , Button
       , buttonBumper, buttonJoystick
       , button1, button2, button3, button4
       , buttonStart
       , controllerEnabled
       -- , numActiveControllers  
       -- , historySize
       , ControllerData(..)
       )
  where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Storable
import qualified Data.Packed.Vector as Vector
import Data.Packed.Vector(Vector)
import qualified Data.Packed.Matrix as Matrix
import Data.Packed.Matrix(Matrix)
import Control.Monad
import Control.Applicative


{-# LINE 36 "Hydra.hsc" #-}

newtype Button = Button { unButton :: CUInt }
               deriving (Eq,Show)
                               
buttonBumper  :: Button
buttonBumper  = Button 128
buttonJoystick  :: Button
buttonJoystick  = Button 256
button1  :: Button
button1  = Button 32
button2  :: Button
button2  = Button 64
button3  :: Button
button3  = Button 8
button4  :: Button
button4  = Button 16
buttonStart  :: Button
buttonStart  = Button 1

{-# LINE 49 "Hydra.hsc" #-}
  
combineButtons :: [Button] -> Button
combineButtons = Button . foldr ((.|.) . unButton) 0

-- | Returns the maximum number of controllers supported by the Sixense control system.
maxControllers :: Int
maxControllers = (fromIntegral :: CInt -> Int) 4
{-# LINE 56 "Hydra.hsc" #-}

data SixenseSuccess = Success | Failure

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
                      , sequenceNumber :: !CUChar
                      , rotQuat :: Matrix Float
                      , firmwareRevision :: !CUShort
                      , hardwareRevision :: !CUShort
                      , packetType :: !CUShort
                      , magneticFrequency :: !CUShort
                      , enabled :: !Bool
                      , controllerIndex :: !CInt
                      , isDocked :: !Bool
                      , whichHand :: !CUChar
                      , hemiTrackingEnabled :: !Bool
                      }
                      real
instance Storable ControllerData where
  sizeOf _ = (104)
{-# LINE 87 "Hydra.hsc" #-}
  alignment _ = alignment (undefined :: CInt) -- alignment should be alignment of largest data type in the C struct (we could also use CFloat here instead)
  peek p = ControllerData 
           <$> (Vector.fromList $ 
                map realToFrac 
                ((peekArray 3 $ (\hsc_ptr -> peekByteOff hsc_ptr 0) p) :: [CFloat]))
{-# LINE 92 "Hydra.hsc" #-}
           <*> undefined -- TODO
           <*> liftM realToFrac ((\hsc_ptr -> peekByteOff hsc_ptr 48) p)
{-# LINE 94 "Hydra.hsc" #-}
           <*> liftM realToFrac ((\hsc_ptr -> peekByteOff hsc_ptr 52) p)
{-# LINE 95 "Hydra.hsc" #-}
           <*> liftM realToFrac ((\hsc_ptr -> peekByteOff hsc_ptr 56) p)
{-# LINE 96 "Hydra.hsc" #-}
           <*> liftM Button ((\hsc_ptr -> peekByteOff hsc_ptr 60) p)
{-# LINE 97 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 64) p)
{-# LINE 98 "Hydra.hsc" #-}
           <*> undefined -- TODO
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 84) p)
{-# LINE 100 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 86) p)
{-# LINE 101 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 88) p)
{-# LINE 102 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 90) p)
{-# LINE 103 "Hydra.hsc" #-}
           <*> liftM (/= 0) (((\hsc_ptr -> peekByteOff hsc_ptr 92) p) :: IO CInt)
{-# LINE 104 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 96) p)
{-# LINE 105 "Hydra.hsc" #-}
           <*> liftM (/= 0) (((\hsc_ptr -> peekByteOff hsc_ptr 100) p) :: IO CUChar) -- ??
{-# LINE 106 "Hydra.hsc" #-}
           <*> ((\hsc_ptr -> peekByteOff hsc_ptr 101) p)
{-# LINE 107 "Hydra.hsc" #-}
           <*> liftM (/= 0) (((\hsc_ptr -> peekByteOff hsc_ptr 102) p) :: IO CUChar)
{-# LINE 108 "Hydra.hsc" #-}
           
  -- strictly speaking, we should not actually need poke         
  poke p x = do
    undefined
    undefined
    (\hsc_ptr -> pokeByteOff hsc_ptr 48) p (joystickX x)
{-# LINE 114 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 52) p (joystickY x)
{-# LINE 115 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 56) p (trigger x)
{-# LINE 116 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 60) p (unButton $ buttons x)
{-# LINE 117 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 64) p (sequenceNumber x)
{-# LINE 118 "Hydra.hsc" #-}
    undefined
    (\hsc_ptr -> pokeByteOff hsc_ptr 84) p (firmwareRevision x)
{-# LINE 120 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 86) p (hardwareRevision x)
{-# LINE 121 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 88) p (packetType x)
{-# LINE 122 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 90) p (magneticFrequency x)
{-# LINE 123 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 92) p (((fromIntegral . fromEnum) :: Bool -> CInt) $ enabled x)
{-# LINE 124 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 96) p (controllerIndex x)
{-# LINE 125 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 100) p (((fromIntegral . fromEnum) :: Bool -> CInt) $ isDocked x)
{-# LINE 126 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 101) p (whichHand x)
{-# LINE 127 "Hydra.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 102) p (((fromIntegral . fromEnum) :: Bool -> CInt) $ hemiTrackingEnabled x)
{-# LINE 128 "Hydra.hsc" #-}
    
                             
data AllControllerData = AllControllerData { controllers :: [ControllerData] }
                                
type ControllerID = Int

foreign import ccall "sixense.h sixenseInit"
  c_sixsenseInit :: IO CInt
                   
-- | Initialize the Sixense library.
-- This function initializes the Sixense library. It must be called at least one time per application. Subsequent calls will have no effect. Once initialized, the other Sixense function calls will work as described until sixenseExit() is called.
sixenseInit :: IO SixenseSuccess
sixenseInit = mFromCInt c_sixsenseInit

foreign import ccall "sixense.h sixenseExit"
  c_sixsenseExit :: IO CInt
                   
-- | Shut down the Sixense library.
-- This shuts down the Sixense library. After this function call, all Sixense API calls will return failure, until sixenseInit() is called again.
sixenseExit :: IO SixenseSuccess
sixenseExit = mFromCInt c_sixsenseExit



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

-- TODO

-- historyLength :: 0-9
-- | Get state of one of the controllers, selecting how far back into a history of the last 10 updates. 
getData :: ControllerID 
          -> Int -- length of the history to obtain. 0-9
          -> IO (Maybe [ControllerData])
getData which historyLength = undefined

-- | Get state of all of the controllers, selecting how far back into a history of the last 10 updates. 
getAllData :: Int -> IO (Maybe [ControllerData])
getAllData historyLength = undefined

-- | Get the most recent state of one of the controllers.
getNewestData :: ControllerID -> IO (Maybe ControllerData)
getNewestData which = undefined

getAllNewestData :: IO (Maybe [ControllerData])
getAllNewestData = undefined


-- TODO: Only implement the functions you actually need so as to not sit on this forever.

{-
SIXENSE_EXPORT int sixenseSetHemisphereTrackingMode( int which_controller, int state );
SIXENSE_EXPORT int sixenseGetHemisphereTrackingMode( int which_controller, int *state );

SIXENSE_EXPORT int sixenseAutoEnableHemisphereTracking( int which_controller );

SIXENSE_EXPORT int sixenseSetHighPriorityBindingEnabled( int on_or_off );
SIXENSE_EXPORT int sixenseGetHighPriorityBindingEnabled( int *on_or_off );

SIXENSE_EXPORT int sixenseTriggerVibration( int controller_id, int duration_100ms, int pattern_id );

SIXENSE_EXPORT int sixenseSetFilterEnabled( int on_or_off );
SIXENSE_EXPORT int sixenseGetFilterEnabled( int *on_or_off );

SIXENSE_EXPORT int sixenseSetFilterParams( float near_range, float near_val, float far_range, float far_val );
SIXENSE_EXPORT int sixenseGetFilterParams( float *near_range, float *near_val, float *far_range, float *far_val );

SIXENSE_EXPORT int sixenseSetBaseColor( unsigned char red, unsigned char green, unsigned char blue );
SIXENSE_EXPORT int sixenseGetBaseColor( unsigned char *red, unsigned char *green, unsigned char *blue );
-}