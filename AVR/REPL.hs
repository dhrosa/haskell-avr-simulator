module AVR.REPL where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.RegFile (prettyRegFile)
import AVR.State

import Data.Word (Word16)

import System.Console.Readline
import System.Exit
import Text.Printf (printf)

data Command = Regs
             | Dissassemble
             | Back
             | Step
             | Quit
             deriving (Eq, Show)

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
      disassemble = unlines $ zipWith disLine [0..] (programMemory state)
      disLine :: Int -> Word16 -> String
      disLine i word = let marker = if (fromIntegral i == oldProgramCounter state)
                                    then "> "
                                    else "  "
                       in printf "%s%04X: %s" marker i (show . decode $ word)
  
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
      
        "dis"  -> addHistory command >> putStrLn disassemble >> again
      
        "back" -> case (back steps) of
          Nothing   -> (putStrLn "Cannot backtrack any further.") >> again
          Just prev -> addHistory command >> simulate prev
        
        "step" -> case (forward steps) of
          Nothing -> (putStrLn "Cannot step any further.") >> again
          Just next -> addHistory command >> simulate next
      
        "quit" -> exitSuccess

        _      -> (putStrLn "Unrecognized command.") >> again


repl :: ProgramMemory -> IO()
repl pmem = (simulate . toZipper . stepUntilDone . initialState $ pmem) >> return ()