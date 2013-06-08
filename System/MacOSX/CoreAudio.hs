
-- | Partial binding to CoreAudio. 
-- At the moment only HostTime and parts of the HAL (Hardware Abstraction Layer) is supported. 
--
-- See <http://developer.apple.com/documentation/MusicAudio/Reference/CACoreAudioReference/AudioHardware/>

{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module System.MacOSX.CoreAudio 
  ( 
    -- * some basic types
    Device
  , Stream
  , AudioValueRange(..)
  , Direction(..) 
    -- * more types
  , AudioDeviceIOProc 
  , AudioDeviceIOProcFloat
  , mkAudioDeviceIOProc
  , AudioTimeStamp(..)
  , SMPTETime(..)
  , AudioBuffer(..)
  , AudioBufferList(..)
  , pokeAudioBufferList
  , peekAudioBufferList
    -- * HostTime
  , audioGetCurrentHostTime
  , audioConvertHostTimeToNanos
  , audioConvertNanosToHostTime
  , audioGetCurrentTimeInNanos
    -- * low-level whatever
  , audioDeviceStart
  , audioDeviceStop
  , audioDeviceAddIOProc
  , audioDeviceRemoveIOProc
    -- * enumerations
  , enumerateAudioDevices
  , enumerateAudioStreams
    -- * properties
  , audioDeviceGetProperty  
  , audioDeviceGetPropertyList  
  , audioDeviceGetPropertyString
  , audioDeviceGetPropertyCFString

{-  
  , audioDeviceGetPropertyUnsafe
  , audioDeviceGetPropertyListUnsafe  
  , audioDeviceGetPropertyStringUnsafe
  , audioDeviceGetPropertyCFStringUnsafe
-}

  , audioDeviceSetProperty  
  , audioDeviceName  

  )
  where

-----

import Control.Monad

import Data.Char (ord)

import Foreign
import Foreign.C

import System.MacOSX.CoreFoundation

----- types

type Device   = UInt32
type Stream   = UInt32

data AudioValueRange = AudioValueRange Float64 Float64 deriving Show
data Direction = In | Out

instance Storable AudioValueRange where
  sizeOf    _ = 16  
  alignment _ = 8
  
  peek p = do
    x <- peek (castPtr p)
    y <- peek (castPtr p `plusPtr` 8)
    return (AudioValueRange x y)
    
  poke p (AudioValueRange x y) = do
    poke (castPtr p) x
    poke (castPtr p `plusPtr` 8) y
 
----- helper functions -----

fromRight :: Either a b -> b
fromRight (Right x) = x

fromJust :: Maybe a -> a
fromJust (Just x) = x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right x) = Just x

eitherToMaybeIO :: Either a b -> IO (Maybe b)
eitherToMaybeIO = return . eitherToMaybe 

liftRight :: (b -> c) -> Either a b -> Either a c
liftRight _ (Left  y) = Left y
liftRight f (Right x) = Right (f x)

liftRightM :: Monad m => (b -> m c) -> Either a b -> m (Either a c)
liftRightM u ei = case ei of
  Left  y -> return $ Left y
  Right x -> do
    z <- u x
    return $ Right z

liftMaybeIO :: (a -> IO b) -> Maybe a -> IO (Maybe b)
liftMaybeIO _ Nothing  = return Nothing
liftMaybeIO f (Just x) = do { y <- f x ; return (Just y) }
    
----- memory "management" -----

data Mem a = Mem Int (ForeignPtr a)

allocMem n = do
  p <- mallocForeignPtrBytes n
  return $ Mem n p
  
withMem :: Mem a -> (Int -> Ptr a -> IO b) -> IO b
withMem (Mem n p) f = withForeignPtr p $ \q -> f n q

memToString :: Mem CChar -> IO String
memToString m = withMem m $ \_ p -> peekCString p 

memToCFString :: Mem CChar -> IO String
memToCFString m = withMem m $ \_ p -> peekCFString (castPtr p) 

memToStorable :: Storable a => Mem a -> IO a
memToStorable m = withMem m $ \_ p -> peek p 

memToStorableList :: forall a. Storable a => Mem a -> IO [a]
memToStorableList m = withMem m $ \n p -> do
  let u = sizeOf (undefined :: a) 
  forM [0..(div n u)-1] $ \i -> peekElemOff p i

---- converting four character IDs and directions

fromFourCharacterID :: String -> UInt32
fromFourCharacterID [a,b,c,d] = 
  (ord32 a `shiftL` 24) + 
  (ord32 b `shiftL` 16) + 
  (ord32 c `shiftL`  8) + 
  (ord32 d            )  
  where
    ord32 :: Char -> UInt32
    ord32 = fromIntegral . ord  

fromDir In  = True
fromDir Out = False

----- HostTime

foreign import ccall unsafe "HostTime.h AudioGetCurrentHostTime"
  audioGetCurrentHostTime :: IO UInt64
  
foreign import ccall unsafe "HostTime.h AudioConvertHostTimeToNanos" 
  audioConvertHostTimeToNanos :: UInt64 -> IO UInt64 
  
foreign import ccall unsafe "HostTime.h AudioConvertNanosToHostTime" 
  audioConvertNanosToHostTime :: UInt64 -> IO UInt64 

audioGetCurrentTimeInNanos :: IO UInt64  
audioGetCurrentTimeInNanos = ( audioGetCurrentHostTime >>= audioConvertHostTimeToNanos )

----- AudioDeviceIOProc -----

-- | Arguments: 
--
--    * @device           :: UInt32@, 
--
--    * @currentTimeStamp :: Ptr AudioTimeStamp@,
--
--    * @input            :: Ptr (AudioBufferList a)@,
--
--    * @inputTimeStamp   :: Ptr AudioTimeStamp@,
--
--    * @output           :: Ptr (AudioBufferList a)@,
--
--    * @outputTimeStamp  :: Ptr AudioTimeStamp@,
--
--    * @clientData       :: Ptr b@.
--
type AudioDeviceIOProc a b 
  =  UInt32 -> Ptr AudioTimeStamp                   
  -> Ptr (AudioBufferList a) -> Ptr AudioTimeStamp
  -> Ptr (AudioBufferList a) -> Ptr AudioTimeStamp
  -> Ptr b                                        
  -> IO OSStatus
  
type AudioDeviceIOProcFloat c = AudioDeviceIOProc Float c

foreign import ccall "wrapper"
  mkAudioDeviceIOProc :: AudioDeviceIOProc a b -> IO (FunPtr (AudioDeviceIOProc a b))
 
----- AudioBuffer -----

data AudioBuffer a = AudioBuffer 
  { ab_NumberChannels :: UInt32 
  , ab_DataByteSize   :: UInt32
  , ab_Data           :: Ptr a
  }

instance Storable (AudioBuffer a) where
  alignment _ = 4
  sizeOf    _ = 8 + sizeOf (undefined :: Ptr a)
  
  poke p x = do
    poke (castPtr p) (ab_NumberChannels x) ; p <- return $ plusPtr p 4
    poke (castPtr p) (ab_DataByteSize   x) ; p <- return $ plusPtr p 4
    poke (castPtr p) (ab_Data           x) 

  peek p = do
    n <- peek (castPtr p) ; p <- return $ plusPtr p 4
    s <- peek (castPtr p) ; p <- return $ plusPtr p 4
    d <- peek (castPtr p) 
    return $ AudioBuffer n s d

----- AudioBufferList -----

-- Keeps track of multiple buffers.
-- 
-- > typedef struct AudioBufferList {
-- >   UInt32 mNumberBuffers;
-- >   AudioBuffer mBuffers[1];
-- > } AudioBufferList;
--
-- Discussion
-- 
-- When audio data is interleaved, only one buffer is needed in the 
-- AudioBufferList; when dealing with multiple mono channels, each will 
-- need its own buffer. This is accomplished by allocating the needed 
-- space and pointing mBuffers to it.

data AudioBufferList a = AudioBufferList [AudioBuffer a]

-- | Returns the number of bytes written.
pokeAudioBufferList :: Ptr (AudioBufferList a) -> AudioBufferList a -> IO Int
pokeAudioBufferList p (AudioBufferList list) = do
  let len = length list
  poke (castPtr p :: Ptr UInt32) (fromIntegral len)
  pokeArray (castPtr p `plusPtr` 4) list
  return (4 + len * sizeOf (undefined::AudioBuffer a))
  
-- | Does not need the length of the list, as it is stored in the memory.
peekAudioBufferList :: Ptr (AudioBufferList a) -> IO (AudioBufferList a)
peekAudioBufferList p = do
  n <- liftM fromIntegral $ peek (castPtr p :: Ptr UInt32)
  liftM AudioBufferList $ peekArray n (castPtr p `plusPtr` 4)
  
----- AudioTimeStamp -----
  
data AudioTimeStamp = AudioTimeStamp
  { ats_SampleTime    :: Float64 
  , ats_HostTime      :: UInt64 
  , ats_RateScalar    :: Float64
  , ats_WordClockTime :: UInt64
  , ats_SMPTETime     :: SMPTETime
  , ats_Flags         :: UInt32 
  , ats_Reserved      :: UInt32
  } 

instance Storable AudioTimeStamp where
  alignment _ = 8
  sizeOf    _ = 64
  
  poke p x = do
    poke (castPtr p) (ats_SampleTime    x) ; p <- return $ plusPtr p 8
    poke (castPtr p) (ats_HostTime      x) ; p <- return $ plusPtr p 8
    poke (castPtr p) (ats_RateScalar    x) ; p <- return $ plusPtr p 8
    poke (castPtr p) (ats_WordClockTime x) ; p <- return $ plusPtr p 8
    poke (castPtr p) (ats_SMPTETime     x) ; p <- return $ plusPtr p 24
    poke (castPtr p) (ats_Flags         x) ; p <- return $ plusPtr p 4
    poke (castPtr p) (ats_Reserved      x) 

  peek p = do
    s <- peek (castPtr p) ; p <- return $ plusPtr p 8
    h <- peek (castPtr p) ; p <- return $ plusPtr p 8
    r <- peek (castPtr p) ; p <- return $ plusPtr p 8
    w <- peek (castPtr p) ; p <- return $ plusPtr p 8
    m <- peek (castPtr p) ; p <- return $ plusPtr p 24
    f <- peek (castPtr p) ; p <- return $ plusPtr p 4
    v <- peek (castPtr p) 
    return $ AudioTimeStamp s h r w m f v
 
kAudioTimeStampSampleTimeValid     = bit 0 :: UInt32
kAudioTimeStampHostTimeValid       = bit 1 :: UInt32
kAudioTimeStampRateScalarValid     = bit 2 :: UInt32
kAudioTimeStampWordClockTimeValid  = bit 3 :: UInt32
kAudioTimeStampSMPTETimeValid      = bit 4 :: UInt32

----- SMPTETime -----

data SMPTETime = SMPTETime 
  { smpte_Counter  :: UInt64 
  , smpte_Type     :: UInt32
  , smpte_Flags    :: UInt32 
  , smpte_Hours    :: SInt16 
  , smpte_Minutes  :: SInt16 
  , smpte_Seconds  :: SInt16
  , smpte_Frames   :: SInt16
  } 

instance Storable SMPTETime where
  alignment _ = 8
  sizeOf    _ = 24

  poke p x = do
    poke (castPtr p) (smpte_Counter x) ; p <- return $ plusPtr p 8
    poke (castPtr p) (smpte_Type    x) ; p <- return $ plusPtr p 4
    poke (castPtr p) (smpte_Flags   x) ; p <- return $ plusPtr p 4
    poke (castPtr p) (smpte_Hours   x) ; p <- return $ plusPtr p 2
    poke (castPtr p) (smpte_Minutes x) ; p <- return $ plusPtr p 2
    poke (castPtr p) (smpte_Seconds x) ; p <- return $ plusPtr p 2
    poke (castPtr p) (smpte_Frames  x) 

  peek p = do
    c <- peek (castPtr p) ; p <- return $ plusPtr p 8
    t <- peek (castPtr p) ; p <- return $ plusPtr p 4
    f <- peek (castPtr p) ; p <- return $ plusPtr p 4
    h <- peek (castPtr p) ; p <- return $ plusPtr p 2
    m <- peek (castPtr p) ; p <- return $ plusPtr p 2
    s <- peek (castPtr p) ; p <- return $ plusPtr p 2
    r <- peek (castPtr p) 
    return $ SMPTETime c t f h m s r
    
----- AudioDeviceIOProc

foreign import ccall safe "AudioHardware.h AudioDeviceAddIOProc" 
  audioDeviceAddIOProc :: Device -> FunPtr (AudioDeviceIOProc a b) -> Ptr b -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioDeviceRemoveIOProc" 
  audioDeviceRemoveIOProc :: Device -> FunPtr (AudioDeviceIOProc a b) -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioDeviceStart" 
  audioDeviceStart :: Device -> FunPtr (AudioDeviceIOProc a b) -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioDeviceStop" 
  audioDeviceStop :: Device -> FunPtr (AudioDeviceIOProc a b) -> IO OSStatus

----- generic wrapper around Audio****GetPropertyInfo & Audio****GetProperty

type GetPropertyInfo = UInt32 -> Ptr UInt32 -> Ptr Boolean -> IO OSStatus
type GetProperty a   = UInt32 -> Ptr UInt32 -> Ptr a       -> IO OSStatus

audioGetPropertyMem :: GetPropertyInfo -> GetProperty a -> String -> IO (Either OSStatus (Mem a))
audioGetPropertyMem getPropertyInfo getProperty id = do
  let id1 = fromFourCharacterID id
  alloca $ \p -> alloca $ \q -> do  
    os1 <- getPropertyInfo id1 p q
    if os1 /=0 
      then return (Left os1)
      else do
        k <- liftM fromIntegral $ peek p :: IO Int
        m <- allocMem k
        os2 <- withMem m $ \_ s -> getProperty id1 p s
        if os2 /=0
          then return $ Left os2
          else return $ Right m

audioGetProperty :: Storable a => GetPropertyInfo -> GetProperty a -> String -> IO (Maybe a)
audioGetProperty gpi gp id = 
  (audioGetPropertyMem gpi gp id) >>= eitherToMaybeIO >>= (liftMaybeIO memToStorable)
          
audioGetPropertyList :: Storable a => GetPropertyInfo -> GetProperty a -> String -> IO (Maybe [a])
audioGetPropertyList gpi gp id = 
  (audioGetPropertyMem gpi gp id) >>= eitherToMaybeIO >>= (liftMaybeIO memToStorableList)

audioGetPropertyString :: GetPropertyInfo -> GetProperty CChar -> String -> IO (Maybe String)
audioGetPropertyString gpi gp id = 
  (audioGetPropertyMem gpi gp id) >>= eitherToMaybeIO >>= (liftMaybeIO memToString)

audioGetPropertyCFString :: GetPropertyInfo -> GetProperty CChar -> String -> IO (Maybe String)
audioGetPropertyCFString gpi gp id = 
  (audioGetPropertyMem gpi gp id) >>= eitherToMaybeIO >>= (liftMaybeIO memToCFString)

{-
audioGetPropertyUnsafe         gpi gp id = liftM fromJust $ audioGetProperty         gpi gp id
audioGetPropertyListUnsafe     gpi gp id = liftM fromJust $ audioGetPropertyList     gpi gp id
audioGetPropertyStringUnsafe   gpi gp id = liftM fromJust $ audioGetPropertyString   gpi gp id
audioGetPropertyCFStringUnsafe gpi gp id = liftM fromJust $ audioGetPropertyCFString gpi gp id
-}

----- AudioHardware -----
  
foreign import ccall safe "AudioHardware.h AudioHardwareGetPropertyInfo" 
  c_AudioHardwareGetPropertyInfo :: UInt32 -> Ptr UInt32 -> Ptr Boolean -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioHardwareGetProperty" 
  c_AudioHardwareGetProperty :: UInt32 -> Ptr UInt32 -> Ptr a -> IO OSStatus
  
audioHardwareGetPropertyList = 
  audioGetPropertyList 
    c_AudioHardwareGetPropertyInfo 
    c_AudioHardwareGetProperty

enumerateAudioDevices :: IO [Device]
enumerateAudioDevices = do
  xx <- audioHardwareGetPropertyList "dev#"  
  case xx of
    Nothing -> do
      fail "enumeration of audio devices failed."
    Just ls -> return ls
    
----- AudioDevice -----

foreign import ccall safe "AudioHardware.h AudioDeviceGetPropertyInfo" 
  c_AudioDeviceGetPropertyInfo 
    :: Device -> UInt32 -> Boolean -> UInt32 -> Ptr UInt32 -> Ptr Boolean -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioDeviceGetProperty" 
  c_AudioDeviceGetProperty 
    :: Device -> UInt32 -> Boolean -> UInt32 -> Ptr UInt32 -> Ptr a -> IO OSStatus

audioDeviceGetPropertyString :: Device -> Int -> Direction -> String -> IO (Maybe String)
audioDeviceGetPropertyString device channel dir = 
  audioGetPropertyString 
    (c_AudioDeviceGetPropertyInfo device ch isInput) 
    (c_AudioDeviceGetProperty     device ch isInput)
  where 
    isInput = fromDir dir 
    ch = fromIntegral channel

audioDeviceGetPropertyCFString :: Device -> Int -> Direction -> String -> IO (Maybe String)
audioDeviceGetPropertyCFString device channel dir = 
  audioGetPropertyCFString 
    (c_AudioDeviceGetPropertyInfo device ch isInput) 
    (c_AudioDeviceGetProperty     device ch isInput)
  where 
    isInput = fromDir dir 
    ch = fromIntegral channel
  
audioDeviceGetPropertyList :: Storable a => Device -> Int -> Direction -> String -> IO (Maybe [a])
audioDeviceGetPropertyList device channel dir =
  audioGetPropertyList
    (c_AudioDeviceGetPropertyInfo device ch isInput) 
    (c_AudioDeviceGetProperty     device ch isInput)
  where 
    isInput = fromDir dir 
    ch = fromIntegral channel

audioDeviceGetProperty 
  :: Storable a 
  => Device       -- ^ device id
  -> Int          -- ^ channel
  -> Direction    -- ^ direction (input\/output)
  -> String       -- ^ four character ID of the property
  -> IO (Maybe a)
audioDeviceGetProperty device channel dir =
  audioGetProperty
    (c_AudioDeviceGetPropertyInfo device ch isInput) 
    (c_AudioDeviceGetProperty     device ch isInput)
  where 
    isInput = fromDir dir 
    ch = fromIntegral channel

{-
audioDeviceGetPropertyUnsafe         d c i id = liftM fromJust $ audioDeviceGetProperty         d c i id
audioDeviceGetPropertyListUnsafe     d c i id = liftM fromJust $ audioDeviceGetPropertyList     d c i id
audioDeviceGetPropertyStringUnsafe   d c i id = liftM fromJust $ audioDeviceGetPropertyString   d c i id
audioDeviceGetPropertyCFStringUnsafe d c i id = liftM fromJust $ audioDeviceGetPropertyCFString d c i id
-}

audioDeviceName :: Device -> IO String
audioDeviceName dev = liftM fromJust $ audioDeviceGetPropertyString dev 0 Out "name" 

foreign import ccall safe "AudioHardware.h AudioDeviceSetProperty"
  c_AudioDeviceSetProperty 
    :: Device -> Ptr AudioTimeStamp -> UInt32 -> Boolean -> UInt32 -> UInt32 -> Ptr a -> IO OSStatus

audioDeviceSetProperty :: Storable a => Device -> UInt32 -> Direction -> String -> a -> IO OSStatus
audioDeviceSetProperty dev channel dir id x = do
  os <- with x $ \p -> c_AudioDeviceSetProperty 
    dev nullPtr channel (fromDir dir) (fromFourCharacterID id) (fromIntegral $ sizeOf x) p
  return os
 
-- | input and output streams
enumerateAudioStreams :: Device -> IO ([Stream],[Stream])
enumerateAudioStreams dev = do
  inp <- audioDeviceGetPropertyList dev 0 In  "stm#"
  out <- audioDeviceGetPropertyList dev 0 Out "stm#"
  return ( fromJust inp , fromJust out )

  
----- AudioStream -----

{-
foreign import ccall safe "AudioHardware.h AudioStreamGetPropertyInfo" 
  c_AudioStreamGetPropertyInfo 
    :: UInt32 -> UInt32 -> UInt32 -> Ptr UInt32 -> Ptr Boolean -> IO OSStatus

foreign import ccall safe "AudioHardware.h AudioStreamGetProperty" 
  c_AudioStreamGetProperty 
    :: UInt32 -> UInt32 -> UInt32 -> Ptr UInt32 -> Ptr a -> IO OSStatus

audioStreamGetPropertyList stream channel = 
  audioGetPropertyList
    (c_AudioStreamGetPropertyInfo stream channel) 
    (c_AudioStreamGetProperty     stream channel)
          
-}
