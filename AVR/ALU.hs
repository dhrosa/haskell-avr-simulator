module AVR.ALU where

import qualified AVR.Types.StatusReg as S

import Data.Bits
import Data.Word (Word8)

-- | Unary ALU operations
data UnaryOpType = Complement -- ^ Flip bits
                 | Negate     -- ^ Two's complement negation
                 | Increment  -- ^ Increment by 1
                 | Decrement  -- ^ Decrement by 1
                 | Test       -- ^ Check if non-zero
                 | Clear      -- ^ Set to zero
                 | Set        -- ^ Set register to 0xFF
                 deriving (Eq, Show)

-- | Binary ALU Operations
data BinaryOpType = Add
                  | AddCarry
                  | Subtract
                  | SubtractCarry
                  | And
                  | Or
                  | Xor
                  deriving (Eq, Show)
                           
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
             
alu :: AluOp -> AluResult
alu NoOp = AluResult 0 S.empty

alu (UnaryOp op a s) = case op of
  _ -> emptyResult