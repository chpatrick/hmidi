
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

import System.IO

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
selectMidiDevice 
  :: MIDIHasName a 
  => String         -- ^ prompt
  -> Maybe String   -- ^ default device name
  -> [a]            -- ^ list of devices
  -> IO a  
selectMidiDevice prompt mbdefault srclist = do
  names <- mapM getName srclist
  let nsrc = length srclist
  putStrLn prompt
  src <- case srclist of
    []  -> do
      putStrLn "no midi devices found"
      fail "no midi devices found"
    [x] -> do
      putStrLn $ "device #1 (" ++ head names ++ ") selected."
      return x
    _  -> do
      k <- case findIndex (==mbdefault) (map Just names) of
        Just i -> return (i+1)
        Nothing -> do
          forM_ (zip [1..] names) $ \(i,name) -> putStrLn $ show i ++ ": " ++ name
          putStr "please select a midi device: "
          hFlush stdout
          l <- getLine
          putStrLn ""
          let k = case maybeRead l of
                    Nothing -> nsrc
                    Just m  -> if m<1 || m>nsrc then nsrc else m
          return k
      putStrLn $ "device #" ++ show k ++ " (" ++ names!!(k-1) ++ ") selected."
      return $ srclist!!(k-1)
  return src
  
-- | Select a MIDI input device (source) 
selectInputDevice :: String -> Maybe String -> IO Source  
selectInputDevice prompt mbdefault = do
  srclist <- enumerateSources
  src <- selectMidiDevice prompt mbdefault srclist
  return src
  
-- | Select a MIDI output device (destination)
selectOutputDevice :: String -> Maybe String -> IO Destination  
selectOutputDevice prompt mbdefault = do
  dstlist <- enumerateDestinations
  dst <- selectMidiDevice prompt mbdefault dstlist
  return dst

--------------------------------------------------------------------------------

  