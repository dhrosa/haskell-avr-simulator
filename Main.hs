module Main where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.RegFile (prettyRegFile)

import AVR.State

import qualified Data.ByteString as B
import Data.Bits

import Data.Word (Word8,  Word16)
import Control.Monad

import System.Console.Readline
import System.Exit
import System.Process (system)
import System.Environment (getArgs)

import Text.Printf (printf)

-- | Re-interpets a list of Word8 (as in from a ByteString unpack) as a list of Word16
word8to16 :: [Word8] -> [Word16]
word8to16 (a:b:rest) = comb : word8to16 rest
  where
    comb = ((fromIntegral b) `shiftL` 8) + (fromIntegral a)
word8to16 [] = []
word8to16 _ = error "input list must have even number of bytes."

type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper x = (x, [])

forward :: Zipper a -> Maybe (Zipper a)
forward ([], _) = Nothing
forward (a:as, bs) = Just (as, a:bs)

back ::  Zipper a -> Maybe (Zipper a)
back (_, []) = Nothing
back (as, b:bs) = Just (b:as, bs) 

current :: Zipper a -> a
current (a:_, _) = a
current _  = error "Cannot take current value of empty zipper."

-- | Executes one simulation step
-- | The return value is a tuple of the instruction executed, and the next state
step :: State -> (Instruction, State)
step state = let inst = decode (fetch state)
             in (inst, exec inst state)

-- | Takes an initial processor state and simulates it until the processor halts.
stepUntilDone :: State -> [(Instruction, State)]
stepUntilDone initial
  = tail $ takeWhile (not . halted . snd) $ iterate (step . snd) (NOP, initial)
    
-- | REPL (read-evaluate-print-loop) for simulator
simulate :: Zipper (Instruction, State) -> IO (Zipper (Instruction, State))
simulate steps = do
  let (inst, state) = current steps
      again = (simulate steps)
  
  putStrLn ""
  putStrLn $ printf "PC = 0x%04X" (oldProgramCounter state)
  print inst
  putStrLn ""
  line  <- readline "> "
  
  case line of
    Nothing -> exitSuccess
    Just command -> 
      case command of 
        "regs" -> addHistory command >> putStrLn (prettyRegFile (regFile state) ++ show (sreg state))  >> again
      
        "back" -> case (back steps) of
          Nothing   -> (putStrLn "Cannot backtrack any further.") >> again
          Just prev -> addHistory command >> simulate prev
        
        "step" -> case (forward steps) of
          Nothing -> (putStrLn "Cannot step any further.") >> again
          Just next -> addHistory command >> simulate next
      
        "quit" -> exitSuccess

        _      -> (putStrLn "Unrecognized command.") >> again

-- | Takes an assembler source frmo stdin and simulates it.
main :: IO()
main = do
  -- assemble the input
  source <- liftM head getArgs
  _ <- system ("cp " ++ source ++ " temp.s")
  _ <- system "avr-as -mmcu=avr1 temp.s -o temp.a"
  _ <- system "avr-ld -mavr1 -Tlinker.x -o temp.elf temp.a"
  _ <- system "avr-objcopy -S -O binary temp.elf temp.bin"
  pmem <- liftM (word8to16 . B.unpack) (B.readFile "temp.bin")
  _ <- system "rm -f temp.s temp.a temp.elf temp.bin"

  _ <- simulate $ toZipper $ stepUntilDone (initialState pmem)
  return ()
