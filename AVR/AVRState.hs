module AVR.AVRState where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16)

type ProgramCounter = Word16

data AVRState = AVRState {
  oldProgramCounter :: ProgramCounter,
  programCounter :: ProgramCounter,
  regFile :: R.RegFile,
  sreg    :: S.StatusReg,
  programMemory :: Vector Word16,
  ioRegs :: Vector Word8,
  ram :: Vector Word8,
  skipInstruction :: Bool,
  cycles :: Integer,
  halted :: Bool
  } deriving (Show)
             
initialState :: Vector Word16 -> AVRState
initialState pmem = AVRState {
  oldProgramCounter = 0,
  programCounter = 0,
  regFile = R.empty,
  sreg = S.empty,
  programMemory = pmem,
  ioRegs = V.replicate 64 0x00,
  ram = V.replicate 256 0x00,
  skipInstruction = False,
  cycles = 0,
  halted = False
  }