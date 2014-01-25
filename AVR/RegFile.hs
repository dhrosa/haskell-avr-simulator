module AVR.RegFile where

import Data.Word (Word8, Word16)

import Text.Printf (printf)

import Data.Bits (shiftL, shiftR, (.&.))
import Data.List (transpose, intercalate)
import Data.List.Split (chunksOf)

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
newtype RegFile = RegFile { regList :: [Reg] }

instance Show RegFile where
  show = prettyRegFile

-- | The register number which holds the lower byte of this address register
addressPairNum :: AddressRegNum -> RegNum
addressPairNum W = R24
addressPairNum X = R26
addressPairNum Y = R28
addressPairNum Z = R30

-- | An regfile filled with zeros
empty :: RegFile
empty = RegFile (replicate 32 0x00)

-- | Retrieves a register
getReg :: RegNum -> RegFile -> Reg
getReg num (RegFile regs) = regs !! (fromEnum num)

-- | Retrieves a register pair, where the specified reg number represents the lower-byte of the pair
getRegPair :: RegNum -> RegFile -> WideReg
getRegPair num rf = (rh `shiftL` 8) + rl
  where
    [rl, rh] = map (fromIntegral . flip getReg rf) [num, succ num]

-- | Retrieves the value of an address register
getAddressReg :: AddressRegNum -> RegFile -> WideReg
getAddressReg = getRegPair . addressPairNum

  -- | Sets a register
setReg :: RegNum -> Word8 -> RegFile -> RegFile
setReg num val (RegFile regs) = RegFile (left ++ [val] ++ right)
  where
    (left, _:right) = splitAt (fromEnum num) regs
    
-- | Sets a pair of registers
setRegPair :: RegNum -> Word16 -> RegFile -> RegFile 
setRegPair num val rf = setReg rh high $ setReg rl low rf
  where
    [rh, rl] = [succ num, num]
    low = fromIntegral (val .&. 0x00FF)
    high = fromIntegral (val `shiftR` 8)
    
-- | Sets the value of an address register
setAddressReg :: AddressRegNum -> Word16 -> RegFile -> RegFile
setAddressReg = setRegPair . addressPairNum
    
-- | Pretty prints a reg file as a table
prettyRegFile :: RegFile -> String
prettyRegFile rf = unlines . map (intercalate " | " . map showReg) $ rows
  where
    rows = transpose . chunksOf 8 . enumFrom $ R0
    showReg num = printf "R%02d: %02X" (fromEnum num) (getReg num rf)