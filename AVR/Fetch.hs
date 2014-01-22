module AVR.Fetch where

import AVR.AVRState

import Data.Word (Word16)

fetch :: AVRState -> Word16
fetch AVRState {programCounter = pc, programMemory = pmem}
  = if (fromIntegral pc) >= length pmem
    then 0xFFFF
    else pmem !! (fromIntegral pc)