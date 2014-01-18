module AVR.Decoder where

import Data.Word (Word8, Word16)
import Data.Bits

import AVR.State
import AVR.RegFile
import qualified AVR.StatusReg as S
import qualified AVR.ALU as A

type Immediate = Word8

data Instruction = ADD  RegNum RegNum
                 | ADC  RegNum RegNum
                 | SUB  RegNum RegNum
                 | SUBI RegNum Immediate
                 | SBC  RegNum RegNum
                 | SBCI RegNum Immediate
                 | AND  RegNum RegNum
                 | ANDI RegNum Immediate
                 | OR   RegNum RegNum
                 | ORI  RegNum Immediate
                 | EOR  RegNum RegNum
                 | COM  RegNum
                 | NEG  RegNum
                 | INC  RegNum
                 | DEC  RegNum
                 | SER  RegNum
                 deriving (Eq, Show)

-- | Emulates verilog's casex syntax. Matches a 16-bit value against a mask composed of 1, 0, and?'s.
-- | ? is a wild-card value. This function ignores any underscores in the pattern
(=?) :: Word16 -> String -> Bool
val =? pattern = and . zipWith bitMatch bits . filter (/= '_') $ pattern
  where
    bits = map (testBit val) [15,14..0]
    bitMatch _ '?' = True
    bitMatch b '1' = b
    bitMatch b '0' = not b
    bitMatch _ _ = error "Unrecognized character in binary pattern."

--  | Interprets a 16-bit word from program memory as a 
decode :: Word16 -> Instruction
decode i
  | i =? "0000_11??_????_????" = ADD  rd rr
  | i =? "0001_11??_????_????" = ADC  rd rr
  | i =? "0001_10??_????_????" = SUB  rd rr
  | i =? "0101_????_????_????" = SUBI rd_high immediate
  | i =? "0100_????_????_????" = SBCI rd_high immediate
  | i =? "0000_10??_????_????" = SBC  rd rr
  | i =? "0010_00??_????_????" = AND  rd rr
  | i =? "0111_????_????_????" = ANDI rd_high immediate
  | i =? "0010_10??_????_????" = OR   rd rr
  | i =? "0110_????_????_????" = ORI  rd_high immediate
  | i =? "0010_01??_????_????" = EOR  rd rr
  | i =? "1001_010?_????_0000" = COM  rd
  | i =? "1001_010?_????_0001" = NEG  rd
  | i =? "1001_010?_????_0011" = INC  rd
  | i =? "1001_010?_????_1010" = DEC  rd
  | i =? "1001_010?_????_1111" = SER  rd
  | otherwise = error "Unimplemented instruction encountered while decoding."
  where
    bits inds = foldl (.|.) 0
                $ zipWith (\val pos -> if val then bit pos else 0) (map (testBit i) inds) [0..]
    
    rr = toEnum $ bits [4..8]
    rd = toEnum $ bits [0,1,2,3,9]
    
    rd_high = toEnum $ (bits [8..11]) `shiftL` 1
    
    immediate = bits ([0..3] ++ [8..11])