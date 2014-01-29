module AVR.Exec where

import AVR.AVRState
import AVR.Decoder
import qualified AVR.ALU as A

import qualified AVR.StatusReg as S

import Data.Word (Word16)
import Data.Bits

data PCUpdate = PCStall
              | PCNext
              | PCNextTwo
              | PCSkip
              | PCOffset Word16
              | PCStack
              | PCIndirect
              | PCAbsolute Word16
              deriving (Eq, Show)
                
data RegFileUpdate = NoRegFileUpdate
                   | RegFileUpdate RegNum
                   | RegFileUpdateMovePair RegNum RegNum
                   | RegFileUpdateAddress AddressRegNum
                   deriving (Eq, Show)

data SRegUpdate = NoSRegUpdate
                | SRegUpdate
                  deriving (Eq, Show)

newtype Cycles = Cycles { getCycles :: Integer }

exec :: Instruction -> AVRState -> AVRState
exec inst state@AVRState{programCounter=pc, sreg=s, cycles=oldCycles, skipInstruction=skip}
  = update $ 
    state { oldProgramCounter = pc,
            programCounter = nextPC,
            sreg = newSreg,
            halted = inst == HALT,
            skipInstruction = skipNext,
            cycles = oldCycles + getCycles cyclesInc
          }
  where
    reg = flip getReg state
    regPair = flip getRegPair state
    addressReg = flip getAddressReg state
    
    aluResult = A.alu aluOp
    aluOutput = A.output aluResult
    wideAluOutput = A.wideOutput aluResult
    
    update = foldl1 (.) [
      updateRf,
      updateIORegs,
      updateMemory,
      updateAddress,
      updateStack
      ]
    
    updateAddress = case inst of
      LD _ raddr incType -> setAddressReg raddr $
                            addressReg raddr + (case incType of
                                                   NoInc -> 0
                                                   PostInc -> 1
                                                   PreDec -> (-1)
                                               )
                            
      ST raddr incType _ -> setAddressReg raddr $
                            addressReg raddr + (case incType of
                                                   NoInc -> 0
                                                   PostInc -> 1
                                                   PreDec -> (-1)
                                               )
      _ -> id
    
    updateMemory = case inst of
      ST raddr incType ra -> writeDMem ((addressReg raddr) - (if incType == PreDec then 1 else 0)) (reg ra)
      STD raddr q ra      -> writeDMem ((addressReg raddr) + q) (reg ra)
      _ -> id
      
    updateRf = if skip then id
                 else case rfUpdate of
                   NoRegFileUpdate -> id
                   RegFileUpdate dest -> setReg dest aluOutput
                   RegFileUpdateMovePair ra rb -> setRegPair ra (regPair rb)
                   RegFileUpdateAddress raddr -> setAddressReg raddr wideAluOutput
      
    updateIORegs = case inst of
      OUT addr ra -> writeIOReg addr (reg ra)
      SBI addr b  -> writeIOReg addr =<< (flip setBit (fromEnum b) . readIOReg addr)
      CBI addr b  -> writeIOReg addr =<< (flip clearBit (fromEnum b) . readIOReg addr)
      _           -> id
      
    updateStack = case inst of
      PUSH ra -> stackPush (reg ra)
      POP  _  -> stackPop
      RCALL _ -> stackPushPC
      ICALL   -> stackPushPC
      CALL _  -> stackPushPC
      RET     -> stackPopPC
      RETI    -> stackPopPC
      _       -> id
      
    newSreg    = if skip then s
                 else case sregUpdate of
                   NoSRegUpdate -> s
                   SRegUpdate   -> A.newSreg aluResult
    
    nextPC     = if skip then (pc + 1)
                 else case pcUpdate of
                   PCStall    -> pc
                   PCNext     -> pc + 1
                   PCNextTwo  -> pc + 2
                   PCSkip     -> pc + 1
                   PCOffset k -> pc + k + 1
                   PCStack    -> stackPeekPC state
                   PCIndirect -> addressReg Z
                   PCAbsolute k -> k
                   
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
                       NoSRegUpdate,
                       PCOffset k,
                       Cycles 2)
                      
      IJMP         -> (A.NoOp,
                       NoRegFileUpdate,
                       NoSRegUpdate,
                       PCIndirect,
                       Cycles 2)
                      
      RCALL k      -> (A.NoOp,
                       NoRegFileUpdate,
                       NoSRegUpdate,
                       PCOffset k,
                       Cycles 3)
                      
      ICALL        -> (A.NoOp,
                       NoRegFileUpdate,
                       NoSRegUpdate,
                       PCStack,
                       Cycles 3)
                      
      CALL k       -> (A.NoOp,
                       NoRegFileUpdate,
                       NoSRegUpdate,
                       PCAbsolute k,
                       Cycles 4)
                      
      RET          -> (A.NoOp,
                       NoRegFileUpdate,
                       NoSRegUpdate,
                       PCStack,
                       Cycles 4)
                      
      RETI         -> (A.BitOp A.FlagSet undefined A.Bit7 s,
                       NoRegFileUpdate,
                       SRegUpdate,
                       PCStack,
                       Cycles 4)
      
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
                     
      LDS ra k    -> (A.UnaryOp A.Identity (readDMem k state) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNextTwo,
                      Cycles 2)
                     
      LD   ra raddr inc -> let address = addressReg raddr - (if inc == PreDec then 1 else 0)
                           in (A.UnaryOp A.Identity (readDMem address state) s,
                               RegFileUpdate ra,
                               NoSRegUpdate,
                               PCNext,
                               Cycles (case inc of NoInc -> 1; PostInc -> 2; PreDec -> 3))
                              
      LDD ra raddr q -> let address = addressReg raddr + q
                        in (A.UnaryOp A.Identity (readDMem address state) s,
                            RegFileUpdate ra,
                            NoSRegUpdate,
                            PCNext,
                            Cycles 2)
                              
      ST   _ _ _ -> (A.NoOp,
                     NoRegFileUpdate,
                     NoSRegUpdate,
                     PCNext,
                     Cycles 2)
      
      STD _ _ _  -> (A.NoOp,
                     NoRegFileUpdate,
                     NoSRegUpdate,
                     PCNext,
                     Cycles 2)
      
      IN   ra io  -> (A.UnaryOp A.Identity (readIOReg io state) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                      
      OUT  _ _  -> (A.NoOp,
                      NoRegFileUpdate,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 1)
                     
      PUSH _     -> (A.NoOp,
                     NoRegFileUpdate,
                     NoSRegUpdate,
                     PCNext,
                     Cycles 2)
                
      POP ra      -> (A.UnaryOp A.Identity (stackPeek state) s,
                      RegFileUpdate ra,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 2)
                   
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
                     
      SBI  _ _    -> (A.NoOp,
                      NoRegFileUpdate,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 2)
                     
      CBI _ _     -> (A.NoOp,
                      NoRegFileUpdate,
                      NoSRegUpdate,
                      PCNext,
                      Cycles 2)
                     
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