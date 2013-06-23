
--
-- A very simple example application using System.MIDI.
--
-- It accepts NoteOn / NoteOff messages from a MIDI source and 
-- sends corresponding chords to a MIDI destination.
--

module Main where

--------------------------------------------------------------------------------

import Control.Monad

import System.MIDI
import System.MIDI.Utility

--------------------------------------------------------------------------------
-- the essence

chord = [0,4,7]
output_channel = 1

mycallback outconn event@(MidiEvent _ (MidiMessage chn msg)) = do
  case msg of
    NoteOff k v -> forM_ chord $ \j -> send outconn $ MidiMessage output_channel $ NoteOff (k+j) v
    NoteOn  k v -> forM_ chord $ \j -> send outconn $ MidiMessage output_channel $ NoteOn  (k+j) v
    _           -> return () 
mycallback _ _ = return ()
      
--------------------------------------------------------------------------------
-- main      
      
main = do

  src <- selectInputDevice Nothing
  dst <- selectOutputDevice Nothing
  
  outconn <- openDestination dst
  inconn  <- openSource src $ Just (mycallback outconn)
  putStrLn "connected"
 
  start inconn ; putStrLn "started. Press 'ENTER' to exit."
  getLine
  stop inconn  ; putStrLn "stopped."
  
  close inconn ; putStrLn "closed."
  close outconn
  
