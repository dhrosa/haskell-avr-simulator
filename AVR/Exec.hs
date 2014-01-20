module AVR.Exec where

import AVR.State
import AVR.Decoder
import qualified AVR.RegFile as R
import qualified AVR.ALU as A

exec :: Instruction -> State -> State
exec inst state@State{programCounter=pc, regFile=rf, sreg=s}
  = state { programCounter = nextPC,
            regFile = newRegFile,
            sreg = newSreg,
            halted = inst == HALT
          }
  where
    reg num = R.getReg num rf
    
    (A.AluResult aluOutput aluSreg) = A.alu aluOp
    newRegFile = if writeRf
                 then R.setReg dest aluOutput rf
                 else rf
    newSreg    = if updateSreg
                 then aluSreg
                 else s
    
    (aluOp, dest, writeRf, updateSreg, nextPC) = case inst of
      -- Arithmetic operations
      ADD  ra rb  -> (A.BinaryOp A.Add (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)
                     
      ADC  ra rb  -> (A.BinaryOp A.AddCarry (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)
                     
      SUB  ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)
                     
      SUBI ra imm -> (A.BinaryOp A.Subtract (reg ra) imm s
                      , ra, True, True, pc + 1)
                                  
      SBC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)

      SBCI ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s
                      , ra, True, True, pc + 1)

      AND  ra rb  -> (A.BinaryOp A.And (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)

      ANDI ra imm -> (A.BinaryOp A.And (reg ra) imm s
                      , ra, True, True, pc + 1)

      OR   ra rb  -> (A.BinaryOp A.Or (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)

      ORI  ra imm -> (A.BinaryOp A.Or (reg ra) imm s
                      , ra, True, True, pc + 1)

      EOR  ra rb  -> (A.BinaryOp A.Xor (reg ra) (reg rb) s
                      , ra, True, True, pc + 1)

      COM  ra     -> (A.UnaryOp A.Complement (reg ra) s
                      , ra, True, True, pc + 1)

      NEG  ra     -> (A.UnaryOp A.Negate (reg ra) s
                      , ra, True, True, pc + 1)

      INC  ra     -> (A.UnaryOp A.Increment (reg ra) s
                      , ra, True, True, pc + 1)

      DEC  ra     -> (A.UnaryOp A.Decrement (reg ra) s
                      , ra, True, True, pc + 1)

      SER  ra     -> (A.UnaryOp A.Set (reg ra) s
                      , ra, True, True, pc + 1)
                     
      -- Branch instructions
      RJMP offset -> (A.NoOp
                      , undefined, False, False, pc + offset)
      
      CP   ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s
                      , undefined, False, True, pc + 1)
                     
      CPC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s
                      , undefined, False, True, pc + 1)
                     
      CPI  ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s
                      , undefined, False, True, pc + 1)
                     
      -- Data Transfer
                     
      MOV  ra rb  -> (A.UnaryOp A.Identity (reg rb) s
                      , ra, True, False, pc + 1)
                     
      LDI  ra imm -> (A.UnaryOp A.Identity imm s
                      , ra, True, False, pc + 1)
                     
      -- Bit Ops
      
      LSR  ra     -> (A.UnaryOp A.LogicalShiftRight (reg ra) s
                      , ra, True, True, pc + 1)
                     
      ROR  ra     -> (A.UnaryOp A.RotateRight (reg ra) s
                      , ra, True, True, pc + 1)
                     
      ASR  ra     -> (A.UnaryOp A.ArithmeticShiftRight (reg ra) s
                      , ra, True, True, pc + 1)
                     
      SWAP ra     -> (A.UnaryOp A.Swap (reg ra) s
                      , ra, True, False, pc + 1)
                     
      BCLR ind    -> (A.BitOp A.FlagClear undefined ind s
                      , undefined, False, True, pc + 1)
                     
      BSET ind    -> (A.BitOp A.FlagSet undefined ind s
                      , undefined, False, True, pc + 1)
                     
      BST  ra ind -> (A.BitOp A.StoreTransfer (reg ra) ind s
                      , undefined, False, True, pc + 1)
                     
      BLD  ra ind -> (A.BitOp A.LoadTransfer (reg ra) ind s
                      , ra, True, False, pc + 1)
                     
      -- MCU Control
                     
      NOP         -> (A.NoOp, undefined, False, False, pc + 1)
                     
      HALT        -> (undefined, undefined, False, False, pc)