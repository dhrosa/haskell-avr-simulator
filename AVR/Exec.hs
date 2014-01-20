module AVR.Exec where

import AVR.State
import AVR.Decoder
import qualified AVR.RegFile as R
import qualified AVR.ALU as A

import Data.Word (Word16)

data PCUpdate = PCStall
              | PCNext
              | PCSkip
              | PCOffset Word16
              deriving (Eq, Show)
                
data RegFileUpdate = NoRegFileUpdate
                   | RegFileUpdate R.RegNum
                   deriving (Eq, Show)

data SRegUpdate = NoSRegUpdate
                | SRegUpdate
                  deriving (Eq, Show)

newtype Cycles = Cycles { getCycles :: Integer }

exec :: Instruction -> State -> State
exec inst state@State{programCounter=pc, regFile=rf, sreg=s, cycles=oldCycles}
  = state { programCounter = nextPC,
            regFile = newRegFile,
            sreg = newSreg,
            halted = inst == HALT,
            skipInstruction = pcUpdate == PCSkip,
            cycles = oldCycles + getCycles cyclesInc
          }
  where
    reg num = R.getReg num rf
    
    (A.AluResult aluOutput aluSreg) = A.alu aluOp
    
    newRegFile = case rfUpdate of
      NoRegFileUpdate -> rf
      RegFileUpdate dest -> R.setReg dest aluOutput rf
      
    newSreg    = case sregUpdate of
      NoSRegUpdate -> s
      SRegUpdate   -> aluSreg
    
    nextPC     = case pcUpdate of
      PCStall    -> pc
      PCNext     -> pc + 1
      PCSkip     -> pc + 1
      PCOffset k -> pc + k + 1
    
    (aluOp, rfUpdate, sregUpdate, pcUpdate, cyclesInc) = case inst of
      -- Arithmetic operations
      ADD  ra rb  -> (A.BinaryOp A.Add (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      ADC  ra rb  -> (A.BinaryOp A.AddCarry (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      SUB  ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      SUBI ra imm -> (A.BinaryOp A.Subtract (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                                  
      SBC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      SBCI ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      AND  ra rb  -> (A.BinaryOp A.And (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      ANDI ra imm -> (A.BinaryOp A.And (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      OR   ra rb  -> (A.BinaryOp A.Or (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      ORI  ra imm -> (A.BinaryOp A.Or (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      EOR  ra rb  -> (A.BinaryOp A.Xor (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      COM  ra     -> (A.UnaryOp A.Complement (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      NEG  ra     -> (A.UnaryOp A.Negate (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      INC  ra     -> (A.UnaryOp A.Increment (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      DEC  ra     -> (A.UnaryOp A.Decrement (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)

      SER  ra     -> (A.UnaryOp A.Set (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      -- Branch instructions
      RJMP k       -> (A.NoOp,
                       NoRegFileUpdate,
                       SRegUpdate,
                       PCOffset k,
                       Cycles 2)
      
      CP   ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      CPC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      CPI  ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      -- Data Transfer
                     
      MOV  ra rb  -> (A.UnaryOp A.Identity (reg rb) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      LDI  ra imm -> (A.UnaryOp A.Identity imm s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      -- Bit Ops
      
      LSR  ra     -> (A.UnaryOp A.LogicalShiftRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      ROR  ra     -> (A.UnaryOp A.RotateRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      ASR  ra     -> (A.UnaryOp A.ArithmeticShiftRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      SWAP ra     -> (A.UnaryOp A.Swap (reg ra) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      BCLR ind    -> (A.BitOp A.FlagClear undefined ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      BSET ind    -> (A.BitOp A.FlagSet undefined ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      BST  ra ind -> (A.BitOp A.StoreTransfer (reg ra) ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      BLD  ra ind -> (A.BitOp A.LoadTransfer (reg ra) ind s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      -- MCU Control
                     
      NOP         -> (A.NoOp, NoRegFileUpdate, NoSRegUpdate, PCNext, Cycles 1)
                     
      HALT        -> (A.NoOp, NoRegFileUpdate, NoSRegUpdate, PCStall, Cycles 1)