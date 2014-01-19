module AVR.RegFile where

import Data.Word (Word8, Word16)

import Text.Printf (printf)

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

-- | The AVR utilizes register pairs for some instructions,
-- | W = R25:R24, X = R27:26, Y = R29:R28, Z = R31:R30
data RegPairNum = W | X | Y | Z
                deriving (Eq, Show)

-- | Registers are 8-bits wide
type Reg     = Word8
-- | Register pairs are 16-bits wide
type WideReg = Word16

-- | Represents the 32 general purpose registers
newtype RegFile = RegFile { regList :: [Reg] }

instance Show RegFile where
  show (RegFile regs) = unlines $ zipWith showReg (enumFrom R0) $ regs
    where
      showReg num val = printf "%s: %02x" (show num) val

-- | An regfile filled with zeros
empty :: RegFile
empty = RegFile (replicate 32 0)

-- | Retrieves a register
getReg :: RegNum -> RegFile -> Reg
getReg num (RegFile regs) = regs !! (fromEnum num)

-- | Sets a register
setReg :: RegNum -> Word8 -> RegFile -> RegFile
setReg num val (RegFile regs) = RegFile (left ++ [val] ++ right)
  where
    (left, _:right) = splitAt (fromEnum num) regs