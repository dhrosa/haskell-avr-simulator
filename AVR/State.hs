module AVR.State where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S

import Data.Word (Word16)

type ProgramCounter = Word16
type ProgramMemory = [Word16]

data State = State {
  oldProgramCounter :: ProgramCounter,
  programCounter :: ProgramCounter,
  regFile :: R.RegFile,
  sreg    :: S.StatusReg,
  programMemory :: ProgramMemory,
  skipInstruction :: Bool,
  cycles :: Integer,
  halted :: Bool
  } deriving (Show)
             
initialState :: ProgramMemory -> State
initialState pmem = State {
  oldProgramCounter = 0,
  programCounter = 0,
  regFile = R.empty,
  sreg = S.empty,
  programMemory = pmem,
  skipInstruction = False,
  cycles = 0,
  halted = False
  }