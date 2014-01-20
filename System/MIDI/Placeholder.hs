
-- |This is just to be able to produce a Haddock documentation on a Linux system

module System.MIDI.Placeholder
  ( module System.MIDI.Base

  , Source
  , Destination
  , Connection
 
  , enumerateSources
  , enumerateDestinations
  
  , MIDIHasName
  , getName
  , getModel
  , getManufacturer

  , openSource
  , openDestination
  , close
  , send
  , sendSysEx
  , start
  , stop

  , getNextEvent
  , checkNextEvent
  , getEvents
  , getEventsUntil
  , currentTime
    
  ) where

import Data.Word
import System.MIDI.Base

placeholder :: a
placeholder = error "Placeholder value for Haddock."

-- |The opaque data type representing a MIDI source.
data Source 

-- |The opaque data type representing a MIDI destination.
data Destination 

-- |The opaque data type representing a MIDI connection.
data Connection 

class MIDIHasName c

instance MIDIHasName Source
instance MIDIHasName Destination

-- |Enumerates the MIDI sources present in the system.
enumerateSources :: IO [Source]
enumerateSources = placeholder

-- |Enumerates the MIDI destinations present in the system.
enumerateDestinations :: IO [Destination]
enumerateDestinations = placeholder

-- |These functions return the name, model and manufacturer of a MIDI source \/ destination.
-- 
-- Note: On Win32, only `getName` returns a somewhat meaningful string at the moment.
getName :: MIDIHasName a => a -> IO String
getModel :: MIDIHasName a => a -> IO String
getManufacturer :: MIDIHasName a => a -> IO String

getName = placeholder
getModel = placeholder
getManufacturer = placeholder

-- |Opens a MIDI Source.
-- There are two possibilites to receive MIDI messages. The user can either supply a callback function,
-- or get the messages from an asynchronous buffer. However, mixing the two approaches is not allowed.
openSource :: Source -> Maybe ClientCallback -> IO Connection 
openSource = placeholder

-- |Opens a MIDI Destination.
openDestination :: Destination -> IO Connection 
openDestination = placeholder

-- |Gets the next event from a buffered connection (see also `openSource`)
getNextEvent :: Connection -> IO (Maybe MidiEvent)
getNextEvent = placeholder

-- | Checks the next event from a buffered connection, but does not remove it from the buffer.
checkNextEvent :: Connection -> IO (Maybe MidiEvent)
checkNextEvent = placeholder

-- | Gets all the events with timestamp less than the specified from the buffer.
getEventsUntil :: Connection -> TimeStamp -> IO [MidiEvent]
getEventsUntil = placeholder

-- |Gets all the events from the buffer (see also `openSource`)
getEvents :: Connection -> IO [MidiEvent]
getEvents = placeholder
      
-- |Sends a short message. The connection must be a `Destination`.
send :: Connection -> MidiMessage -> IO ()
send = placeholder

-- |Sends a system exclusive message. You shouldn't include the starting \/ trailing bytes 0xF0 and 0xF7.
-- 
-- Note: On Win32, the connection must be a `Destination`
sendSysEx :: Connection -> [Word8] -> IO ()
sendSysEx = placeholder
 
-- |Starts a connection. This is required for receiving MIDI messages, and also for starting the clock.
start :: Connection -> IO ()
start = placeholder

-- |Stops a connection.
stop :: Connection -> IO ()
stop = placeholder
  
-- |Closes a MIDI Connection.
close :: Connection -> IO ()
close = placeholder
 
-- |Returns the time elapsed since the last `start` call, in milisecs.
currentTime :: Connection -> IO Word32
currentTime = placeholder
 
