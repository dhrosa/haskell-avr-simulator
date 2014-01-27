module AVR.AVRState where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S

import Data.Bits
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Word (Word8, Word16)

import Control.Monad

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

-- | Reads a value from the data memory, which maps the register file, io regs, and SRAM
readDMem :: Word16 -> AVRState -> Word8
readDMem addr state
  | addr < 32        = R.getReg rnum (regFile state)
  | (addr - 32) < 64 = (ioRegs state) ! ioAddr
  | otherwise        = (ram state) ! ramAddr
    where
      rnum = toEnum $ fromIntegral addr
      ioAddr = fromIntegral $ addr - 32
      ramAddr = fromIntegral $ addr - 96

writeDMem :: Word16 -> Word8 -> AVRState -> AVRState
writeDMem addr val state
  | addr < 32         = state {regFile = newRf}
  | (addr -  32) < 64 = state {ioRegs = newIORegs}
  | otherwise         = state {ram = newRam}
  where
    newRf     = R.setReg (toEnum $ fromIntegral addr) val (regFile state)
    newIORegs = (ioRegs state) // [(fromIntegral (addr - 32), val)]
    newRam = (ram state) // [(fromIntegral (addr - 96), val)]
    
getSP :: AVRState -> Word16
getSP state = (sph `shiftL` 8) + spl
  where
    sph = fromIntegral $ readDMem 0x5E state
    spl = fromIntegral $ readDMem 0x5D state
    
setSP :: Word16 -> AVRState -> AVRState
setSP sp = writeDMem 0x5E sph . writeDMem 0x5D spl
  where
    sph = fromIntegral $ sp `shiftR` 8
    spl = fromIntegral $ sp .&. 0x00FF

incSP :: AVRState -> AVRState
incSP = setSP =<< (+1) . getSP

decSP :: AVRState -> AVRState
decSP = setSP =<< (subtract 1) . getSP

stackPush :: Word8 -> AVRState -> AVRState
stackPush val = incSP . (writeDMem' val =<< getSP)
  where writeDMem' = flip writeDMem

stackPop ::  AVRState -> (Word8, AVRState)
stackPop = liftM2 (,) (readDMem =<< getSP) decSP