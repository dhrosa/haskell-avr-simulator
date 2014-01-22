module AVR.Fetch where

import AVR.AVRState

import Data.Word (Word16)

fetch :: AVRState -> Word16
fetch AVRState {programCounter = pc, programMemory = pmem}
  | pc' >= length pmem = 0xFFFF
  | otherwise          = pmem !! pc'
  where pc' = fromIntegral pc
