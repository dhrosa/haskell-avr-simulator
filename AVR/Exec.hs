module AVR.Exec where

import AVR.AVRState
import AVR.Decoder
import qualified AVR.RegFile as R
import qualified AVR.ALU as A

import qualified AVR.StatusReg as S

import Data.Word (Word16)
import Data.Bits

import Data.Vector ((!), (//))

data PCUpdate = PCStall
              | PCNext
              | PCSkip
              | PCOffset Word16
              deriving (Eq, Show)
                
data RegFileUpdate = NoRegFileUpdate
                   | RegFileUpdate R.RegNum
                   | RegFileUpdateMovePair R.RegNum R.RegNum
                   | RegFileUpdateAddress R.AddressRegNum
                   deriving (Eq, Show)

data SRegUpdate = NoSRegUpdate
                | SRegUpdate
                  deriving (Eq, Show)

newtype Cycles = Cycles { getCycles :: Integer }

exec :: Instruction -> AVRState -> AVRState
exec inst state@AVRState{programCounter=pc, regFile=rf, sreg=s, cycles=oldCycles, skipInstruction=skip}
  = state { oldProgramCounter = pc,
            programCounter = nextPC,
            ioRegs = newIORegs,
            regFile = newRegFile,
            sreg = newSreg,
            halted = inst == HALT,
            skipInstruction = skipNext,
            cycles = oldCycles + getCycles cyclesInc
          }
  where
    reg = flip R.getReg rf
    regPair = flip R.getRegPair rf
    addressReg = flip R.getAddressReg rf
    
    aluResult = A.alu aluOp
    aluOutput = A.output aluResult
    wideAluOutput = A.wideOutput aluResult
    
    newRegFile = if skip then rf
                 else case rfUpdate of
                   NoRegFileUpdate -> rf
                   RegFileUpdate dest -> R.setReg dest aluOutput  rf
                   RegFileUpdateMovePair ra rb -> R.setRegPair ra (regPair rb) rf
                   RegFileUpdateAddress raddr -> R.setAddressReg raddr wideAluOutput rf
      
    newSreg    = if skip then s
                 else case sregUpdate of
                   NoSRegUpdate -> s
                   SRegUpdate   -> A.newSreg aluResult
    
    nextPC     = if skip then (pc + 1)
                 else case pcUpdate of
                   PCStall    -> pc
                   PCNext     -> pc + 1
                   PCSkip     -> pc + 1
                   PCOffset k -> pc + k + 1
                   
    newIORegs = case inst of 
      OUT io ra -> (ioRegs state) // [(fromIntegral io, reg ra)]
      _         -> (ioRegs state)
                   
    skipNext = if skip then False
               else (pcUpdate == PCSkip)
    
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
                     
      ADIW raddr k -> (A.WideOp A.AddWide (addressReg raddr) k s,
                         RegFileUpdateAddress raddr,
                         SRegUpdate,
                         PCNext,
                         Cycles 2)
                     
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

      SBIW raddr k -> (A.WideOp A.SubtractWide (addressReg raddr) k s,
                         RegFileUpdateAddress raddr,
                         SRegUpdate,
                         PCNext,
                         Cycles 2)

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
      
      CPSE ra rb  -> let equal = (reg ra) == (reg rb)
                     in (A.NoOp,
                         NoRegFileUpdate,
                         NoSRegUpdate,
                         if equal then PCSkip else PCNext,
                         Cycles 1)
      
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
                     
      SBRC ra ind -> let cleared = testBit (reg ra) (fromEnum ind) == False
                     in (A.NoOp,
                         NoRegFileUpdate,
                         NoSRegUpdate,
                         if cleared then PCSkip else PCNext,
                         Cycles 1)
                        
      SBRS ra ind -> let set = testBit (reg ra) (fromEnum ind)
                     in (A.NoOp,
                         NoRegFileUpdate,
                         NoSRegUpdate,
                         if set then PCSkip else PCNext,
                         Cycles 1)
      
      BRBS ind k  -> let set = S.test s (fromEnum ind)
                     in (A.NoOp,
                         NoRegFileUpdate,
                         NoSRegUpdate,
                         if set then PCOffset (fromIntegral k) else PCNext,
                         Cycles 1)
                        
      BRBC ind k  -> let cleared = S.test s (fromEnum ind) == False
                     in (A.NoOp,
                         NoRegFileUpdate,
                         NoSRegUpdate,
                         if cleared then PCOffset (fromIntegral k) else PCNext,
                         Cycles 1)
      
      -- Data Transfer
                     
      MOV  ra rb  -> (A.UnaryOp A.Identity (reg rb) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      MOVW ra rb  -> (A.NoOp,
                      RegFileUpdateMovePair ra rb,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      LDI  ra imm -> (A.UnaryOp A.Identity imm s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      IN   ra io  -> (A.UnaryOp A.Identity (ioRegs state ! (fromEnum io)) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                      
      OUT  _ _  -> (A.NoOp,
                      NoRegFileUpdate,
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