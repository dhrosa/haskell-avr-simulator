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
                 | FlagClear  -- ^ Clears the given status register bit
                 | FlagSet    -- ^ Sets the given status register bit
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
                           
data AluOp = NoOp |
             UnaryOp { unaryOpType :: UnaryOpType,
                       operand     :: Word8,
                       sreg        :: S.StatusReg
                     } |
             BinaryOp { binaryOpType :: BinaryOpType,
                        operandA     :: Word8,
                        operandB     :: Word8,
                        sreg         :: S.StatusReg
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
                    
  FlagClear  -> AluResult 0 (S.set s (a .&. 7) False)
  
  FlagSet    -> AluResult 0 (S.set s (a .&. 7) True )
  
  _          -> error "Unimplemented unary ALU operation encountered."
  
  
