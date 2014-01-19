module AVR.Exec where

import AVR.State
import AVR.Decoder
import qualified AVR.RegFile as R
import qualified AVR.ALU as A

exec :: Instruction -> State -> State
exec inst State{regFile=rf, sreg=s} = State { pc = 0,
                                              regFile = newRegFile,
                                              sreg = newSreg
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
    
    (aluOp, dest, writeRf, updateSreg) = case inst of
      ADD  ra rb  -> (A.BinaryOp A.Add (reg ra) (reg rb) s
                      , ra, True, True)
                     
      ADC  ra rb  -> (A.BinaryOp A.AddCarry (reg ra) (reg rb) s
                      , ra, True, True)
                     
      SUB  ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s
                      , ra, True, True)
                     
      SUBI ra imm -> (A.BinaryOp A.Subtract (reg ra) imm s
                      , ra, True, True)
                                  
      SBC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s
                      , ra, True, True)

      SBCI ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s
                      , ra, True, True)

      AND  ra rb  -> (A.BinaryOp A.And (reg ra) (reg rb) s
                      , ra, True, True)

      ANDI ra imm -> (A.BinaryOp A.And (reg ra) imm s
                      , ra, True, True)

      OR   ra rb  -> (A.BinaryOp A.Or (reg ra) (reg rb) s
                      , ra, True, True)

      ORI  ra imm -> (A.BinaryOp A.Or (reg ra) imm s
                      , ra, True, True)

      EOR  ra rb  -> (A.BinaryOp A.Xor (reg ra) (reg rb) s
                      , ra, True, True)

      COM  ra     -> (A.UnaryOp A.Complement (reg ra) s
                      , ra, True, True)

      NEG  ra     -> (A.UnaryOp A.Negate (reg ra) s
                      , ra, True, True)

      INC  ra     -> (A.UnaryOp A.Increment (reg ra) s
                      , ra, True, True)

      DEC  ra     -> (A.UnaryOp A.Decrement (reg ra) s
                      , ra, True, True)

      SER  ra     -> (A.UnaryOp A.Set (reg ra) s
                      , ra, True, True)
                     
      CP   ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s
                      , undefined, False, True)
                     
      CPC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s
                      , undefined, False, True)
                     
      CPI  ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s
                      , undefined, False, True)
                     
      MOV  ra rb  -> (A.UnaryOp A.Identity (reg rb) s
                      , ra, True, False)
                     
      LDI  ra imm -> (A.UnaryOp A.Identity imm s
                      , ra, True, False)
                     
      LSR  ra     -> (A.UnaryOp A.LogicalShiftRight (reg ra) s
                      , ra, True, True)
                     
      ROR  ra     -> (A.UnaryOp A.RotateRight (reg ra) s
                      , ra, True, True)
                     
      ASR  ra     -> (A.UnaryOp A.ArithmeticShiftRight (reg ra) s
                      , ra, True, True)
                     
      SWAP ra     -> (A.UnaryOp A.Swap (reg ra) s
                      , ra, True, False)
                     
      BCLR ind    -> (A.BitOp A.FlagClear undefined ind s
                      , undefined, False, True)
                     
      BSET ind    -> (A.BitOp A.FlagSet undefined ind s
                      , undefined, False, True)
                     
      BST  ra ind -> (A.BitOp A.StoreTransfer (reg ra) ind s
                      , undefined, False, True)
                     
      BLD  ra ind -> (A.BitOp A.LoadTransfer (reg ra) ind s
                      , ra, True, False)
                     
      NOP         -> (A.NoOp, undefined, False, False)
                     
      _ -> undefined