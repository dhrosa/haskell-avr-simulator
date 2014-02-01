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

import Control.Monad

type REPLState = Zipper (Instruction, AVRState)
type EvalError = String

-- | Executes one simulation step
-- | The return value is a tuple of the instruction executed, and the next state
step :: AVRState -> (Instruction, AVRState)
step state = let inst = decode (fetch state)
             in (inst, exec inst state)

-- | Takes an initial processor state and simulates it until the processor halts.
stepUntilDone :: AVRState -> [(Instruction, AVRState)]
stepUntilDone initial
  = tail $ takeWhile (not . halted . snd) $ iterate (step . snd) (NOP, initial)

evaluate :: String -> REPLState -> (Either EvalError String, REPLState)
evaluate line replState = case parse parseCommand "(unknown)" line of
  Left err -> (Left (show err), replState)
  Right command -> case command of
    
    Regs -> (Right (prettyRegFile state ++ show (sreg state)), replState)
    
    Back -> case (back replState) of
      Nothing -> (Left "Cannot backtrack any further.", replState)
      Just prev -> (Right (printInst (current prev)), prev)
      
    Step -> case (forward replState) of
      Nothing -> (Left "Cannot step any further.", replState)
      Just next -> (Right (printInst (current next)), next)
      
    IORegs -> (Right printIORegs, replState)
    
    Quit -> (Left "Quit", replState)
    
    PMem start end -> (Right (printPMem start end), replState)
    
    SP -> (Right (printf "SP = 0x%04X" (getSP state)), replState)
    
  where
    (_, state) = current replState
    
    printPMem start end = unlines $ map pMemLine [start..end]
    pMemLine i = printf "%04X: %04X" i $ readPMem16 (fromIntegral i) state
      
    printIORegs = unlines $ V.toList $ V.imap ioLine (ioRegs state)
    ioLine i byte = printf "%02x: %02x" i byte
    
    printInst (i,s) = printf "PC = 0x%04X\n%s" (oldProgramCounter s) (show i)

loop :: REPLState -> IO (REPLState)
loop replState = do
  line <- readline "> "
  case line of 
    Nothing -> exitSuccess
    Just command -> do
      let (result, nextReplState) = evaluate command replState
      case result of
        Left msg -> putStrLn  $ "Error: " ++ msg
        Right msg -> addHistory command >> putStrLn msg
      return nextReplState
  
iterateM :: (Monad m) => (a -> m a) -> a -> m a
iterateM = foldr (>=>) return . repeat
  
repl :: Vector Word16 -> IO()
repl pmem = (iterateM loop . toZipper . stepUntilDone . initialState $ pmem) >> return ()