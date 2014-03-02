-- | Operations on the AVR's status register
module AVR.StatusReg where

-- | The AVR uses the following flags for many operations, such as branching and arithmetic
data StatusReg = StatusReg {
  carry :: Bool,     -- ^ Whether the previous arithmetic operation has a carry
  zero :: Bool,      -- ^ Whether the previous operation's result was zero
  negative :: Bool,  -- ^ Whether the previous operation had a negative result
  overflow :: Bool,  -- ^ Whether the previous operation had a two's complement overflow
  sign :: Bool,      -- ^ The XOR of negative and overflow
  halfCarry :: Bool, -- ^ Whether the previous arithmetic opertation's lower 4-bits has a carry
  transfer :: Bool,  -- ^ The transfer bit, used for the BLD and BST instructions
  interrupt :: Bool  -- ^ Whether interrupts are enabled
  } deriving (Eq)
  
-- | A blank status register
empty :: StatusReg
empty = StatusReg False False False False False False False False

-- | Retrieves bits from the sreg at the given index
test :: StatusReg -> Int -> Bool
test s 0 = carry s
test s 1 = zero s
test s 2 = negative s
test s 3 = overflow s
test s 4 = sign s
test s 5 = halfCarry s
test s 6 = transfer s
test s 7 = interrupt s
test _ _ = error "Bit index into sreg must be in range 0-7"

-- | Sets a bit in the sreg at the given index
set :: StatusReg -> Int -> Bool -> StatusReg
set s 0 v = s {carry = v}
set s 1 v = s {zero = v}
set s 2 v = s {negative = v}
set s 3 v = s {overflow = v}
set s 4 v = s {sign = v}
set s 5 v = s {halfCarry = v}
set s 6 v = s {transfer = v}
set s 7 v = s {interrupt = v}
set _ _ _ = error "Bit index into sreg must be in range 0-7"

instance Show StatusReg where
  show (StatusReg c z n v s h t i) = "SREG: "
                                     ++ p i "I"
                                     ++ p t "T"
                                     ++ p h "H"
                                     ++ p s "S"
                                     ++ p v "V"
                                     ++ p n "N"
                                     ++ p z "Z"
                                     ++ p c "C"
    where p flag str = if flag then str else "_"

  
