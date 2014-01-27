module AVR.Decoder where

import Data.Word (Word8, Word16)
import Data.Bits

import AVR.AVRState (RegNum, AddressRegNum (..))
import qualified AVR.ALU as A

import Text.Printf (printf)

type Immediate = Word8
type WideImmediate = Word16
type Offset = Word16
type IOAddress = Word8

data AddressInc = NoInc
                | PostInc
                | PreDec
                deriving (Eq, Enum, Show)

data Instruction =
  -- Arithmetic instructions
  ADD  RegNum RegNum
  | ADC  RegNum RegNum
  | ADIW AddressRegNum WideImmediate
  | SUB  RegNum RegNum
  | SUBI RegNum Immediate
  | SBC  RegNum RegNum
  | SBCI RegNum Immediate
  | SBIW AddressRegNum WideImmediate
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
    -- Branch instructions
  | RJMP Offset
  | CPSE RegNum RegNum
  | CP   RegNum RegNum
  | CPC  RegNum RegNum
  | CPI  RegNum Immediate
  | SBRC RegNum A.BitIndex
  | SBRS RegNum A.BitIndex
  | BRBS A.BitIndex Offset
  | BRBC A.BitIndex Offset
    -- Data transfer instructions
  | MOV  RegNum RegNum
  | MOVW RegNum RegNum
  | LDI  RegNum Immediate
  | LD   RegNum AddressRegNum AddressInc
  | ST   AddressRegNum AddressInc RegNum
  | IN   RegNum IOAddress
  | OUT  IOAddress RegNum
    -- Bit and bit-test instructions
  | LSR  RegNum
  | ROR  RegNum
  | ASR  RegNum
  | SWAP RegNum
  | BSET A.BitIndex
  | BCLR A.BitIndex
  | BST  RegNum A.BitIndex
  | BLD  RegNum A.BitIndex
    -- MCU Control Instructions
  | NOP
  | HALT
  deriving (Eq, Show)

-- | Number of words in program memory occupied by an instruction
instructionWords :: (Num a) => Instruction -> a
instructionWords _ = 1

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
  | i =? "1001_0110_????_????" = ADIW addressReg wideImmediate
  | i =? "0001_10??_????_????" = SUB  rd rr
  | i =? "0101_????_????_????" = SUBI rd_high immediate
  | i =? "0100_????_????_????" = SBCI rd_high immediate
  | i =? "0000_10??_????_????" = SBC  rd rr
  | i =? "1001_0111_????_????" = SBIW addressReg wideImmediate
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
                                 
  | i =? "1100_????_????_????" = RJMP offset12
  | i =? "0001_00??_????_????" = CPSE rd rr
  | i =? "0001_01??_????_????" = CP   rd rr
  | i =? "0000_01??_????_????" = CPC  rd rr
  | i =? "0011_????_????_????" = CPI  rd_high immediate
  | i =? "1111_110?_????_0???" = SBRC rd bitIndex
  | i =? "1111_111?_????_0???" = SBRS rd bitIndex
  | i =? "1111_00??_????_????" = BRBS bitIndex offset7
  | i =? "1111_01??_????_????" = BRBC bitIndex offset7
                                 
  | i =? "0010_11??_????_????" = MOV  rd rr
  | i =? "0000_0001_????_????" = MOVW rdPair rrPair
  | i =? "1110_????_????_????" = LDI  rd_high immediate
  | i =? "1001_000?_????_11??" = LD   rd X addressInc
  | i =? "1000_000?_????_1000" = LD   rd Y NoInc
  | i =? "1001_000?_????_10??" = LD   rd Y addressInc
  | i =? "1000_000?_????_0000" = LD   rd Z NoInc
  | i =? "1001_000?_????_00??" = LD   rd Z addressInc
  | i =? "1001_001?_????_11??" = ST   X addressInc rd
  | i =? "1000_001?_????_1000" = ST   Y NoInc rd
  | i =? "1001_001?_????_10??" = ST   Y addressInc rd
  | i =? "1000_001?_????_0000" = ST   Z NoInc rd
  | i =? "1001_001?_????_00??" = ST   Z addressInc rd
  | i =? "1011_0???_????_????" = IN   rd ioAddr
  | i =? "1011_1???_????_????" = OUT  ioAddr rd
                                 
  | i =? "1001_010?_????_0110" = LSR  rd
  | i =? "1001_010?_????_0111" = ROR  rd
                                 
  | i =? "1001_010?_????_0101" = ASR  rd
  | i =? "1001_010?_????_0010" = SWAP rd
  | i =? "1001_0100_0???_1000" = BSET s
  | i =? "1001_0100_1???_1000" = BCLR s
                                 
  | i =? "1111_101?_????_0???" = BST rd bitIndex
  | i =? "1111_100?_????_0???" = BLD rd bitIndex
                                 
  | i =? "0000_0000_0000_0000" = NOP
  | i =? "1111_1111_1111_1111" = HALT
  | otherwise = error $ printf "Unimplemented instruction encountered while decoding: 0x%016X" i
  where
    bits inds = foldl (.|.) 0
                $ zipWith (\val pos -> if val then bit pos else 0) (map (testBit i) inds) [0..]
    
    rd = toEnum $ bits [4..8]
    rr = toEnum $ bits [0,1,2,3,9]
    
    rd_high = toEnum $ (bits [4..7]) `setBit` 4
    
    immediate = bits ([0..3] ++ [8..11])
    
    s = toEnum $ bits [4..6]
    bitIndex = toEnum $ bits [0..2]

    signExtend12 x = if testBit x 11
                     then x .|. 0xF000
                     else x

    signExtend7  x = if testBit x 6
                     then x .|. 0xFF80
                     else x

    offset7  = signExtend7 (bits [3..9])

    -- Sign extended 12-bit constant
    offset12 = signExtend12 (bits [0..11])
    
    ioAddr = bits ([0..3] ++ [9, 10])
    
    rdPair = toEnum $ bits [4..7] `shiftL` 1
    rrPair = toEnum $ bits [0..3] `shiftL` 1
    
    addressReg = toEnum $ bits [4, 5]
    wideImmediate = bits ([0..3] ++ [6, 7])
    
    addressInc = toEnum $ bits [0, 1]