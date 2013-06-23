
--
-- A very simple example application using System.MIDI.
-- It's a basic MIDI monitor: prints all the incoming messages.
--

module Main where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent

import System.MIDI
import System.MIDI.Utility

--------------------------------------------------------------------------------
-- the essence

mythread conn = do
  events <- getEvents conn
  mapM_ print events
  (threadDelay 5000)
  mythread conn
     
--------------------------------------------------------------------------------
-- main      
      
main = do

  src <- selectInputDevice Nothing

  conn <- openSource src Nothing
  putStrLn "connected"

  threadid <- forkIO (mythread conn) 
 
  start conn ; putStrLn "started. Press 'ENTER' to exit."
  getLine
  stop conn    ; putStrLn "stopped."
  
  killThread threadid
  
  close conn   ; putStrLn "closed."
  
