
-- | MIDI sync to an external clock source.
--
-- To avoid confusion:
-- In our terminology, /beat/ means a quarter note (the same thing as the B in BPM).
-- In MIDI terminology however, a \"MIDI beat\" means a sixteenth note.
--
-- With our notion of beats, one bar is 4 beats (in 4/4 signature, that is)
--

module System.MIDI.Sync 
  ( Beats, BPM, openSourceWithSync
  ) 
  where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import System.MIDI

--------------------------------------------------------------------------------

-- | Song position measured in beats (that is, quarter notes), starting from zero.
-- So with 120 BPM, you will have song position 120 after one minute.
type Beats = Double

-- | Estimated BPM
type BPM = Double

oneTwentyFourth = 1/24 :: Double
lambda          = 0.05 :: Double  -- ad-hoc speed of bpm adjustement

-- | Opens a midi source with the possibility to sync to it. 
-- 
-- The user callback gets the the song position in /beats/, 
-- and also we return functions to query to song position and 
-- the estimated BPM. You may want to round the BPM to the nearest 
-- integer if that is appropriate. Song position is Nothing when
-- the playback is stopped.
--
-- Note that when first used, it may need some time to calibrate 
-- the bpm correctly, so start your MIDI host, press play, and 
-- wait a few second. Afterwards, it should be reasonably ok.
-- Also if you do fast realtime BPM changes, 
-- it will be a tiny little bit behind.
--
-- Note that we forward all messages (including clock messages) to 
-- the user, so you can implement your own handling of transport
-- (start/stop/continue) or send messages on clock if you want.
--  
openSourceWithSync 
  :: Source                                     -- ^ midi source
  -> (Maybe Beats -> MidiEvent -> IO ())        -- ^ user callback
  -> IO (Connection, IO (Maybe Beats), IO BPM)  -- ^ (connection, song_position, estimated_bpm) 
openSourceWithSync src userCallback = do

  theLastPos   <- newMVar 0       :: IO (MVar Beats)      -- last song position 
  theBpmEst    <- newMVar 120     :: IO (MVar BPM)        -- last bpm estimation
  thePlayFlag  <- newMVar False   :: IO (MVar Bool)       -- whether we are playing or stopped
  theLastClock <- newMVar 0       :: IO (MVar TimeStamp)  -- timestamp of last clock signal  
  theLastQuery <- newMVar 0       :: IO (MVar Beats)      -- last queried pos 
  
  let queryPos tstamp = do
        b <- readMVar thePlayFlag
        if b 
          then do 
            lastpos   <- readMVar theLastPos    -- song position at the last clock/start message
            bpm       <- readMVar theBpmEst     -- estimated bpm
            lastclock <- readMVar theLastClock  -- time of the last clock/start message
            let tdiff = fromIntegral (tstamp - lastclock) / 60000.0 :: Double   -- in minutes
            let newpos0 = lastpos + tdiff * bpm  -- extrapolate since last clock signal 
            lastquery <- takeMVar theLastQuery
            let newpos = max lastquery newpos0   -- make it monotone in time (!)
            putMVar theLastQuery newpos
            return (Just newpos) 
          else return Nothing
              
  let queryBPM = readMVar theBpmEst
        
  let handle (MidiEvent tstamp msg) = case msg of
  
        SongPosition midibeats -> do
          let pos = fromIntegral midibeats / 6
          replaceMVar theLastPos   pos
          replaceMVar theLastQuery pos
  
        SRTStart -> do
          replaceMVar theLastPos   0
          replaceMVar theLastQuery 0
          replaceMVar theLastClock tstamp
          replaceMVar thePlayFlag  True
  
        SRTStop     -> replaceMVar thePlayFlag False
        
        SRTContinue	-> do
          replaceMVar theLastClock tstamp
          replaceMVar thePlayFlag  True
  
        Reset -> do 
          replaceMVar theLastPos   0
          replaceMVar theLastQuery 0
          replaceMVar thePlayFlag  False
          replaceMVar theBpmEst    120
          
        SRTClock -> do
          lastclock <- takeMVar theLastClock
          bpm       <- takeMVar theBpmEst
          lastpos   <- takeMVar theLastPos
          let lastpos' = lastpos + oneTwentyFourth
          let tdiff = fromIntegral (tstamp - lastclock) / 60000.0 :: Double   -- in minutes
          let bpm' = (1-lambda)*bpm + lambda*(oneTwentyFourth/tdiff)                        
          putMVar theLastClock tstamp
          putMVar theLastPos lastpos'
          putMVar theBpmEst  bpm'      
          print (bpm',tdiff,1/24/tdiff) 
          
        _ -> return ()

  let syncCallback event@(MidiEvent tstamp _) = do
        handle event
        mbpos <- queryPos tstamp
        userCallback mbpos event
          
  conn <- openSource src (Just syncCallback)
  return (conn, currentTime conn >>= queryPos, queryBPM)

--------------------------------------------------------------------------------

replaceMVar :: MVar a -> a -> IO ()      
replaceMVar mv x = do
  _ <- tryTakeMVar mv  
  putMVar mv x
       
--------------------------------------------------------------------------------
