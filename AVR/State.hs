module AVR.State where

import Data.Word (Word8, Word16)

import AVR.RegFile (RegFile)
import AVR.StatusReg (StatusReg)

type PC = Word16

data State = State {
  pc :: PC,
  regFile :: RegFile
  }