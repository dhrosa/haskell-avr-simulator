module AVR.Fetch where

import AVR.AVRState

import Data.Word (Word16)
import Data.Vector ((!?))

fetch :: AVRState -> Word16
fetch AVRState {programCounter = pc, programMemory = pmem} =
  case (pmem !? (fromIntegral pc)) of
    Nothing  -> 0xFFFF
    Just val -> val
