module AVR.Fetch where

import AVR.State

import Data.Word (Word16)

fetch :: State -> Word16
fetch State {programCounter = pc, programMemory = pmem}
  = if (fromIntegral pc) >= length pmem
    then 0xFFFF
    else pmem !! (fromIntegral pc)