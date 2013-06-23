
-- | Helper functions to make it easy to start messing around

module System.MIDI.Utility
  ( selectMidiDevice
  , selectInputDevice
  , selectOutputDevice
  ) 
  where

--------------------------------------------------------------------------------

import Data.List

import Control.Monad
import Control.Concurrent

import System.MIDI
import System.MIDI.Base

--------------------------------------------------------------------------------

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of 
  [(x,"")] -> Just x
  _        -> Nothing

-- | Utility function to help choosing a midi device.
-- If there is only a single device, we select that.
-- You can also set a default device (by its name), which
-- will be automatically selected if present.
selectMidiDevice :: MIDIHasName a => Maybe String -> [a] -> IO a  
selectMidiDevice mbdefault srclist = do
  names <- mapM getName srclist
  forM_ (zip [1..] names) $ \(i,name) -> putStrLn $ show i ++ ": " ++ name
  let nsrc = length srclist
  src <- case srclist of
    []  -> fail "no midi devices found"
    [x] -> return x
    _  -> do
      k <- case findIndex (==mbdefault) (map Just names) of
        Just i -> return (i+1)
        Nothing -> do
          putStrLn "please select a midi device"
          l <- getLine
          let k = case maybeRead l of
                    Nothing -> nsrc
                    Just m  -> if m<1 || m>nsrc then nsrc else m
          return k
      putStrLn $ "device #" ++ show k ++ " (" ++ names!!(k-1) ++ ") selected."
      return $ srclist!!(k-1)
  return src
  
-- | Select a MIDI input device (source) 
selectInputDevice :: Maybe String -> IO Source  
selectInputDevice mbdefault = do
  srclist <- enumerateSources
  putStrLn "midi sources:"
  src <- selectMidiDevice mbdefault srclist
  return src
  
-- | Select a MIDI output device (destination)
selectOutputDevice :: Maybe String -> IO Destination  
selectOutputDevice mbdefault = do
  dstlist <- enumerateDestinations
  putStrLn "\nmidi destinations:"
  dst <- selectMidiDevice mbdefault dstlist
  return dst

--------------------------------------------------------------------------------

  