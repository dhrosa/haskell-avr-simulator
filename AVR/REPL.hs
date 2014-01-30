module AVR.REPL where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.AVRState

import Data.Word (Word16)
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Console.Readline
import System.Exit
import Text.Printf (printf)

import AVR.REPL.Parser
import AVR.REPL.Zipper

-- | Executes one simulation step
-- | The return value is a tuple of the instruction executed, and the next state
step :: AVRState -> (Instruction, AVRState)
step state = let inst = decode (fetch state)
             in (inst, exec inst state)

-- | Takes an initial processor state and simulates it until the processor halts.
stepUntilDone :: AVRState -> [(Instruction, AVRState)]
stepUntilDone initial
  = tail $ takeWhile (not . halted . snd) $ iterate (step . snd) (NOP, initial)

-- | REPL (read-evaluate-print-loop) for simulator
simulate :: Zipper (Instruction, AVRState) -> IO (Zipper (Instruction, AVRState))
simulate steps = do
  let (inst, state) = current steps
      again = (simulate steps)
                          
      printPMem start end = unlines $ map pMemLine [start..end]
      pMemLine i = printf "%04X: %04X" i $ readPMem16 (fromIntegral i) state
      
      printIORegs = unlines $ V.toList $ V.imap ioLine (ioRegs state)
      ioLine i byte = printf "%02x: %02x" i byte
  
  putStrLn ""
  putStrLn $ printf "PC = 0x%04X" (oldProgramCounter state)
  print inst
  putStrLn ""
  line  <- readline "> "
  
  case line of
    Nothing -> exitSuccess
    Just commandStr -> 
      case parse parseCommand "(unknown)" commandStr of 
        Left err -> print err >> again
        Right command -> addHistory commandStr >> (
          case command of 
            Regs -> putStrLn (prettyRegFile state ++ show (sreg state))  >> again
      
            Back -> case (back steps) of
              Nothing   -> putStrLn "Cannot backtrack any further." >> again
              Just prev -> simulate prev
        
            Step -> case (forward steps) of
              Nothing -> putStrLn "Cannot step any further." >> again
              Just next -> simulate next
      
            IORegs -> putStrLn printIORegs >> again
          
            Quit -> putStrLn "Exiting." >> exitSuccess
            
            PMem start end -> putStrLn (printPMem start end) >> again
            
            SP ->  putStrLn (printf "SP = 0x%04X" (getSP state)) >> again
          )

repl :: Vector Word16 -> IO()
repl pmem = (simulate . toZipper . stepUntilDone . initialState $ pmem) >> return ()