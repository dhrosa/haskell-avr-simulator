module AVR.AVRState where

import qualified AVR.StatusReg as S

import Data.Bits
import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import Text.Printf (printf)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Word (Word8, Word16)

import Control.Monad

----------------
-- DATA TYPES --
----------------

type ProgramCounter = Word16

-- | The AVR has 32 general purpose registers
data RegNum =  R0 |  R1 |  R2 |  R3 |
               R4 |  R5 |  R6 |  R7 |
               R8 |  R9 | R10 | R11 |
              R12 | R13 | R14 | R15 |
              R16 | R17 | R18 | R19 |
              R20 | R21 | R22 | R23 |
              R24 | R25 | R26 | R27 |
              R28 | R29 | R30 | R31
            deriving (Eq, Enum, Show)

-- | The AVR utilizes the following register pairs for addressing
-- | W = R25:R24, X = R27:26, Y = R29:R28, Z = R31:R30
data AddressRegNum = W | X | Y | Z
                 deriving (Eq, Enum, Show)

-- | Registers are 8-bits wide
type Reg     = Word8
-- | Register pairs are 16-bits wide
type WideReg = Word16

-- | Represents the 32 general purpose registers
type RegFile = [Reg]

type IOAddress = Word8
type RamAddress = Word16

data AVRState = AVRState {
  oldProgramCounter :: ProgramCounter,
  programCounter :: ProgramCounter,
  regFile :: RegFile,
  sreg    :: S.StatusReg,
  programMemory :: Vector Word16,
  ioRegs :: Vector Word8,
  ram :: Vector Word8,
  skipInstruction :: Bool,
  cycles :: Integer,
  halted :: Bool
  } deriving (Show)

-- | The starting state of the processor, with the given program memory
initialState :: Vector Word16 -> AVRState
initialState pmem = AVRState {
  oldProgramCounter = 0,
  programCounter = 0,
  regFile = replicate 32 0x00,
  sreg = S.empty,
  programMemory = pmem,
  ioRegs = V.replicate 64 0x00,
  ram = V.replicate 256 0x00,
  skipInstruction = False,
  cycles = 0,
  halted = False
  }

--------------------------------
-- REGISTER FILE MANIPULATION --
--------------------------------

-- | The register number which holds the lower byte of this address register
addressPairNum :: AddressRegNum -> RegNum
addressPairNum W = R24
addressPairNum X = R26
addressPairNum Y = R28
addressPairNum Z = R30

-- | Retrieves the value of a register
getReg :: RegNum -> AVRState -> Reg
getReg num (AVRState {regFile=regs}) = regs !! (fromEnum num)

-- | Retrieves a register pair, where the specified reg number represents the lower-byte of the pair
getRegPair :: RegNum -> AVRState -> WideReg
getRegPair num state = (rh `shiftL` 8) + rl
  where
    [rl, rh] = map (fromIntegral . flip getReg state) [num, succ num]

-- | Retrieves the value of an address register
getAddressReg :: AddressRegNum -> AVRState -> WideReg
getAddressReg = getRegPair . addressPairNum

  -- | Sets a register
setReg :: RegNum -> Word8 -> AVRState -> AVRState
setReg num val state = state {regFile = left ++ [val] ++ right}
  where
    regs = regFile state
    (left, _:right) = splitAt (fromEnum num) regs
    
-- | Sets a pair of registers
setRegPair :: RegNum -> Word16 -> AVRState -> AVRState
setRegPair num val = setReg rh high . setReg rl low
  where
    [rh, rl] = [succ num, num]
    low = fromIntegral (val .&. 0x00FF)
    high = fromIntegral (val `shiftR` 8)
    
-- | Sets the value of an address register
setAddressReg :: AddressRegNum -> Word16 -> AVRState -> AVRState
setAddressReg = setRegPair . addressPairNum
    
-- | Pretty prints a reg file as a table
prettyRegFile :: AVRState -> String
prettyRegFile state = unlines . map (intercalate " | " . map showReg) $ rows
  where
    rows = transpose . chunksOf 8 . enumFrom $ R0
    showReg num = printf "R%02d: %02X" (fromEnum num) (getReg num state)

-------------------------
-- MEMORY MANIPULATION --
-------------------------

-- | Reads an IO register
readIOReg :: IOAddress -> AVRState -> Word8
readIOReg addr state = ioRegs state ! (fromIntegral addr)

-- | Writes to an IO register
writeIOReg :: IOAddress -> Word8 -> AVRState -> AVRState
writeIOReg addr val state = state {ioRegs = ioRegs'}
  where
    ioRegs' = (ioRegs state) // [(fromIntegral addr, val)]

-- | Reads a location in SRAM
readRam :: RamAddress -> AVRState -> Word8
readRam addr state = ram state ! (fromIntegral addr)

-- | Writes to a location in SRAM
writeRam :: RamAddress -> Word8 -> AVRState -> AVRState
writeRam addr val state = state {ram = ram'}
  where
    ram' = ram state // [(fromIntegral addr, val)]

-- | Reads a value from the data memory, which maps the register file, io regs, and SRAM
readDMem :: Word16 -> AVRState -> Word8
readDMem addr
  | addr < 32        = getReg rnum
  | (addr - 32) < 64 = readIOReg ioAddr
  | otherwise        = readRam ramAddr
    where
      rnum = toEnum $ fromIntegral addr
      ioAddr = fromIntegral $ addr - 32
      ramAddr = fromIntegral $ addr - 96

-- | Writes a value to the reg file, io regs, or ram, depending on the address
writeDMem :: Word16 -> Word8 -> AVRState -> AVRState
writeDMem addr
  | addr < 32         = setReg rnum
  | (addr -  32) < 64 = writeIOReg ioAddr
  | otherwise         = writeRam ramAddr
  where
    rnum = toEnum $ fromIntegral addr
    ioAddr = fromIntegral $ addr - 32
    ramAddr = fromIntegral $ addr - 96
    
------------------------
-- STACK MANIPULATION --
------------------------
    
-- | The current stack pointer value
getSP :: AVRState -> Word16
getSP state = (sph `shiftL` 8) + spl
  where
    sph = fromIntegral $ readIOReg 0x3E state
    spl = fromIntegral $ readIOReg 0x3D state
    
-- | Sets the stack pointer
setSP :: Word16 -> AVRState -> AVRState
setSP sp = writeIOReg 0x3E sph . writeIOReg 0x3D spl
  where
    sph = fromIntegral $ sp `shiftR` 8
    spl = fromIntegral $ sp .&. 0x00FF

-- | Increments the stack pointer by one
incSP :: AVRState -> AVRState
incSP = setSP =<< (+1) . getSP

-- | Decrements the stack pointer by one
decSP :: AVRState -> AVRState
decSP = setSP =<< (subtract 1) . getSP

-- | Pushes a value onto the stack, this also increments the stack pointer
stackPush :: Word8 -> AVRState -> AVRState
stackPush val = incSP . (writeDMem' val =<< getSP)
  where writeDMem' = flip writeDMem

-- | Pops a value off the stack, this also decrements the stack pointer
stackPop ::  AVRState -> (Word8, AVRState)
stackPop = liftM2 (,) stackPeek decSP

-- | Looks at the value at the top of the stack
stackPeek :: AVRState -> Word8
stackPeek = readDMem =<< (subtract 1) . getSP