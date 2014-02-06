{-# LANGUAGE GADTs #-}
module AVR.REPL where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.AVRState

import Data.Word (Word16)
import Data.Vector (Vector)

import System.Console.Readline
import System.Exit

import AVR.REPL.Command
import AVR.REPL.Expr
import AVR.REPL.Zipper

import Control.Monad

import Text.Parsec (parse)

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
evaluate line replState = case parse parseCommand "repl" line of
  Left err -> (Left (show err), replState)
  
  Right command -> case command of
    Print8 expr -> (Right $ show $ eval expr state, replState)
    Print16 expr -> (Right $ show $ eval expr state, replState)
    
    Step   n    -> case (forward n replState) of
      Nothing -> (Left "Cannot step any further.", replState)
      Just next -> (Right (show . fst . current $ next), next)
                   
    Back   n    -> case (back n replState) of
      Nothing -> (Left "Cannot backtrack any further.", replState)
      Just prev -> (Right (show . fst . current $ prev), prev)
      
    Set8 target val -> let updateState = case target of 
                             TargetReg num -> setReg num (eval val state)
                       in (Right "", updateCurrent (inst, updateState state) replState)
    where
      (inst, state) = current replState

loop :: REPLState -> IO (REPLState)
loop replState = do
  line <- readline "> "
  case line of 
    Nothing -> exitSuccess
    Just command -> do
      let (result, nextReplState) = evaluate command replState
      case result of
        Left err -> putStrLn  $ "Error: " ++ err
        Right msg -> addHistory command >> putStrLn msg
      return nextReplState
  
iterateM :: (Monad m) => (a -> m a) -> a -> m a
iterateM = foldr (>=>) return . repeat
  
repl :: Vector Word16 -> IO()
repl pmem = (iterateM loop . toZipper . stepUntilDone . initialState $ pmem) >> return ()