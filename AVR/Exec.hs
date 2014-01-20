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

exec :: Instruction -> State -> State
exec inst state@State{programCounter=pc, regFile=rf, sreg=s}
  = state { programCounter = nextPC,
            regFile = newRegFile,
            sreg = newSreg,
            halted = inst == HALT,
            skipInstruction = pcUpdate == PCSkip
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
    
    (aluOp, rfUpdate, sregUpdate, pcUpdate) = case inst of
      -- Arithmetic operations
      ADD  ra rb  -> (A.BinaryOp A.Add (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      ADC  ra rb  -> (A.BinaryOp A.AddCarry (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      SUB  ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      SUBI ra imm -> (A.BinaryOp A.Subtract (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                                  
      SBC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      SBCI ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      AND  ra rb  -> (A.BinaryOp A.And (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      ANDI ra imm -> (A.BinaryOp A.And (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      OR   ra rb  -> (A.BinaryOp A.Or (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      ORI  ra imm -> (A.BinaryOp A.Or (reg ra) imm s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      EOR  ra rb  -> (A.BinaryOp A.Xor (reg ra) (reg rb) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      COM  ra     -> (A.UnaryOp A.Complement (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      NEG  ra     -> (A.UnaryOp A.Negate (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      INC  ra     -> (A.UnaryOp A.Increment (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      DEC  ra     -> (A.UnaryOp A.Decrement (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)

      SER  ra     -> (A.UnaryOp A.Set (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      -- Branch instructions
      RJMP k       -> (A.NoOp,
                       NoRegFileUpdate,
                       SRegUpdate,
                       PCOffset k)
      
      CP   ra rb  -> (A.BinaryOp A.Subtract (reg ra) (reg rb) s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      CPC  ra rb  -> (A.BinaryOp A.SubtractCarry (reg ra) (reg rb) s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      CPI  ra imm -> (A.BinaryOp A.SubtractCarry (reg ra) imm s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      -- Data Transfer
                     
      MOV  ra rb  -> (A.UnaryOp A.Identity (reg rb) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext)
                     
      LDI  ra imm -> (A.UnaryOp A.Identity imm s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext)
                     
      -- Bit Ops
      
      LSR  ra     -> (A.UnaryOp A.LogicalShiftRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      ROR  ra     -> (A.UnaryOp A.RotateRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      ASR  ra     -> (A.UnaryOp A.ArithmeticShiftRight (reg ra) s,
                      RegFileUpdate ra,
                      SRegUpdate,
                      PCNext)
                     
      SWAP ra     -> (A.UnaryOp A.Swap (reg ra) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext)
                     
      BCLR ind    -> (A.BitOp A.FlagClear undefined ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      BSET ind    -> (A.BitOp A.FlagSet undefined ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      BST  ra ind -> (A.BitOp A.StoreTransfer (reg ra) ind s,
                      NoRegFileUpdate,
                      SRegUpdate,
                      PCNext)
                     
      BLD  ra ind -> (A.BitOp A.LoadTransfer (reg ra) ind s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext)
                     
      -- MCU Control
                     
      NOP         -> (A.NoOp, NoRegFileUpdate, NoSRegUpdate, PCNext)
                     
      HALT        -> (A.NoOp, NoRegFileUpdate, NoSRegUpdate, PCStall)