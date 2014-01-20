module Main where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)

import AVR.State

import qualified Data.ByteString as B
import Data.Bits

import Data.Word (Word8,  Word16)
import Control.Monad

import System.Process (system)

-- | Re-interpets a list of Word8 (as in from a ByteString unpack) as a list of Word16
word8to16 :: [Word8] -> [Word16]
word8to16 (a:b:rest) = comb : word8to16 rest
  where
    comb = ((fromIntegral b) `shiftL` 8) + (fromIntegral a)
word8to16 [] = []
word8to16 _ = error "input list must have even number of bytes."

-- | Executes one simulation step
-- | The return value is a tuple of the instruction executed, and the next state
step :: State -> (Instruction, State)
step state = let inst = decode (fetch state)
             in (inst, exec inst state)

-- | Takes an initial processor state and simulates it until the processor halts.
stepUntilDone :: State -> [(Instruction, State)]
stepUntilDone initial
  = takeWhile (not . halted . snd) $ iterate (step . snd) (NOP, initial)
    
-- | Takes an assembler source frmo stdin and simulates it.
main :: IO()
main = do
  -- assemble the input
  assemblySource <- getContents
  writeFile "temp.s" assemblySource
  _ <- system "avr-as -mmcu=avr1 temp.s -o temp.elf"
  _ <- system "avr-objcopy -S -O binary temp.elf temp.bin"
  pmem <- liftM (word8to16 . B.unpack) (B.readFile "temp.bin")
  _ <- system "rm -f temp.s temp.elf temp.bin"
  print $ stepUntilDone (initialState pmem)