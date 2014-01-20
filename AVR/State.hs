module AVR.State where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S

import Data.Word (Word16)

type ProgramCounter = Word16
type ProgramMemory = [Word16]

data State = State {
  programCounter :: ProgramCounter,
  regFile :: R.RegFile,
  sreg    :: S.StatusReg,
  programMemory :: ProgramMemory,
  halted :: Bool
  } deriving (Show)
             
initialState :: ProgramMemory -> State
initialState pmem = State {
  programCounter = 0,
  regFile = R.empty,
  sreg = S.empty,
  programMemory = pmem,
  halted = False
  }