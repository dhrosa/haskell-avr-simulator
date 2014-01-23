module AVR.AVRState where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S

import Data.Vector (Vector)

import Data.Word (Word16)

type ProgramCounter = Word16
type ProgramMemory = Vector Word16

data AVRState = AVRState {
  oldProgramCounter :: ProgramCounter,
  programCounter :: ProgramCounter,
  regFile :: R.RegFile,
  sreg    :: S.StatusReg,
  programMemory :: ProgramMemory,
  skipInstruction :: Bool,
  cycles :: Integer,
  halted :: Bool
  } deriving (Show)
             
initialState :: ProgramMemory -> AVRState
initialState pmem = AVRState {
  oldProgramCounter = 0,
  programCounter = 0,
  regFile = R.empty,
  sreg = S.empty,
  programMemory = pmem,
  skipInstruction = False,
  cycles = 0,
  halted = False
  }