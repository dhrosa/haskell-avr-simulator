module AVR.Types where

import Data.Word (Word8, Word16)

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
data WideRegNum = W | X | Y | Z
                deriving (Eq, Show)

-- | Registers are 8-bits wide
type Reg     = Word8
-- | Register pairs are 16-bits wide
type WideReg = Word16
              
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
                  deriving (Show, Eq)
                           
