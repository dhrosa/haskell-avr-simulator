module AVR.ALU where

import qualified AVR.StatusReg as S

import Data.Bits
import Data.Word (Word8)

-- | Unary ALU operations
data UnaryOpType = Complement -- ^ Flip bits
                 | Negate     -- ^ Two's complement negation
                 | Increment  -- ^ Increment by 1
                 | Decrement  -- ^ Decrement by 1
                 | Set        -- ^ Set register to 0xFF
                 | LogicalShiftRight
                 | ArithmeticShiftRight
                 | Swap
                 | Identity
                 deriving (Eq, Enum, Show)

-- | Binary ALU Operations
data BinaryOpType = Add
                  | AddCarry
                  | Subtract
                  | SubtractCarry
                  | And
                  | Or
                  | Xor
                  deriving (Eq, Enum, Show)
                           
data BitIndex = Bit0 | Bit1 | Bit2 | Bit3 | Bit4 | Bit5 | Bit6 | Bit7
              deriving (Eq, Enum, Show)
                           
data BitOpType = FlagClear  -- ^ Clears the given status register bit
               | FlagSet    -- ^ Sets the given status register bit
               | StoreTransfer
               | LoadTransfer
               deriving (Eq, Enum, Show)

data AluOp = NoOp |
             UnaryOp { unaryOpType :: UnaryOpType,
                       operand     :: Word8,
                       sreg        :: S.StatusReg
                     } |
             BinaryOp { binaryOpType :: BinaryOpType,
                        operandA     :: Word8,
                        operandB     :: Word8,
                        sreg         :: S.StatusReg
                      } |
             BitOp {bitOpType :: BitOpType,
                    operand   :: Word8,
                    bitIndex  :: BitIndex,
                    sreg      :: S.StatusReg
                   }
           deriving (Eq, Show)
                    
data AluResult = AluResult {
  output :: Word8,
  newSreg   :: S.StatusReg
  } deriving (Eq, Show)
                    
emptyResult :: AluResult
emptyResult = AluResult 0 S.empty

defaultUpdate :: S.StatusReg -> Bool -> Word8 -> S.StatusReg
defaultUpdate s v val = s {S.zero     = z,
                           S.negative = n,
                           S.overflow = v,
                           S.sign     = n /= v}
  where
    z = val == 0
    n = testBit val 7

alu :: AluOp -> AluResult
alu NoOp = emptyResult

alu (UnaryOp op a s) = case op of
  Complement -> let val = complement a
                in AluResult val $ (defaultUpdate s False val) {S.carry = True}
                   
  Negate     -> let val = -a
                    v = and ((testBit val 7) : map (not . testBit val) [0..6])
                    c = or (map (testBit val) [0..7])
                in AluResult val $ (defaultUpdate s v val) {S.carry = c}
                   
  Increment  -> let val = a + 1
                    v = and ((testBit val 7) : map (not . testBit val) [0..6])
                in AluResult val $ (defaultUpdate s v val)
                   
  Decrement  -> let val = a - 1
                    v   = and ((not (testBit val 7)) : map (testBit val) [0..6])
                in AluResult val $ (defaultUpdate s v val)
                   
  Set        -> AluResult 0xFF s
  
  LogicalShiftRight -> let val = clearBit (a `shiftR` 1) 7
                           c = testBit a 0
                           v = (testBit val 7) /= c
                       in AluResult val $ (defaultUpdate s v val) {S.carry = c}
                          
  ArithmeticShiftRight -> let val = a `shiftR` 1
                              c = testBit a 0
                              v = (testBit val 7) /= c
                          in AluResult val $ (defaultUpdate s v val) {S.carry = c}
  
  Swap                 -> let hi = (a `shiftR` 4) .&. 0x0F
                              lo = a .&. 0x0F
                              val = (lo `shiftL` 4) .|. hi
                          in AluResult val s
  
  Identity              -> AluResult a s
  
  --_          -> error "Unimplemented unary ALU operation encountered."
  
alu (BinaryOp op a b s) = case op of
  Add           -> let val = a + b
                       h = (a3 && b3) || (a3 && nbit7 val) || (nbit7 val && b3)
                       v = (a7 && b7 && nbit7 val) || (na7 && nb7 && bit7 val)
                       c = (a7 && b7) || (a7 && nbit7 val) || (b7 && nbit7 val)
                   in AluResult val $ (defaultUpdate s v val) {S.halfCarry = h, S.carry = c}
                   
  AddCarry      -> alu $ BinaryOp Add (a+carryVal) b s
                   
  Subtract      -> let val = a - b
                       h = (na3 && b3) || (b3 && bit3 val) || (na3 && bit3 val)
                       v = (a7 && nb7 && nbit7 val) || (na7 && b7 && bit7 val)
                       c = (na7 && b7) || (b7 && bit7 val) || (bit7 val && na7)
                       z = if val == 0 then S.carry s else False
                   in AluResult val $ (defaultUpdate s v val) {S.carry = c, S.halfCarry = h, S.zero = z}
                   
  SubtractCarry -> alu $ BinaryOp Subtract (a-carryVal) b s

  And           -> let val = a .&. b
                   in AluResult val $ (defaultUpdate s False val)
                      
  Or            -> let val = a .|. b
                   in AluResult val $ (defaultUpdate s False val)

  Xor           -> let val = a `xor` b
                   in AluResult val $ (defaultUpdate s False val)

  where
    bit3 :: Word8 -> Bool
    bit7 :: Word8 -> Bool
    bit3 x = testBit x 3; bit7 x = testBit x 7
    nbit3 = not . bit3; nbit7 = not . bit7
    a3 = bit3 a; na3 = nbit3 a
    b3 = bit3 b;
    a7 = bit7 a; na7 = nbit7 a
    b7 = bit7 b; nb7 = nbit7 b

    carryVal = if S.carry s then 1 else 0
    
alu (BitOp op a ind s) = case op of
  FlagClear  -> AluResult 0 (S.set s (fromEnum ind) False)
  
  FlagSet    -> AluResult 0 (S.set s (fromEnum ind) True )

  StoreTransfer -> AluResult 0 (s {S.transfer = (testBit a (fromEnum ind))})

  LoadTransfer -> let t = S.transfer s
                      val = if t
                            then setBit   a (fromEnum ind)
                            else clearBit a (fromEnum ind)
                  in AluResult val s