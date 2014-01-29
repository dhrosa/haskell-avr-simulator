module AVR.Fetch where

import AVR.AVRState

import Data.Word (Word16)
import Control.Monad

fetch :: AVRState -> (Word16, Word16)
fetch = liftM2 (,)
        (readPMem16 =<< programCounter)
        (readPMem16 =<< ((+1) .programCounter))
        
-- fetch = do
--   first  <- readPMem16 =<< programCounter
--   second <- readPMem16 =<< ((+1) . programCounter)
--   return (first, second)
  
