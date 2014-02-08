{-# LANGUAGE GADTs #-}
module AVR.REPL where

import AVR.Decoder (decode, Instruction)
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.AVRState

import Data.Word (Word16)
import Data.Vector (Vector)

import System.Console.Readline
import System.Exit

import AVR.REPL.Command
import AVR.REPL.Expr

import Control.Monad
import Text.Parsec (parse)

import Text.Printf (printf)

type EvalError = String

fetchDecode :: AVRState -> Instruction
fetchDecode = decode . fetch

-- | Executes one simulation step
-- | The return value is a tuple of the instruction executed, and the next state
step :: AVRState -> AVRState
step = exec =<< fetchDecode

evaluate :: String -> [AVRState] -> (Either EvalError String, [AVRState])
evaluate _ [] = error "Cannot evaluate with empty history, this shouldn't happen."
evaluate line history@(current:past) = case parse parseCommand "repl" line of
  Left err -> (Left (show err), history)
  
  Right command -> case command of
    Inst        -> (Right (showCurrentInst history), history)
    
    Print8 expr -> (Right $ show $ eval expr current, history)
    
    Print16 expr -> (Right $ show $ eval expr current, history)
    
    Step   n    -> let newHistory = (iterate (\h -> step (head h) : h) history) !! n
                   in (Right (showCurrentInst history), newHistory)
                   
    Back   n    -> if n >= length history
                   then (Left "Cannot backtrack any further.", [last history])
                   else let newHistory = drop n history
                        in (Right (showCurrentInst newHistory), newHistory)
      
    Set8 target val -> let updateState = case target of 
                             TargetReg num -> setReg num =<< eval val
                             TargetPCL -> setPC =<< (onLow . const .  eval val) `ap` getPC
                             TargetPCH -> setPC =<< (onHigh . const .  eval val) `ap` getPC
                             TargetIO addr -> writeIOReg addr =<< eval val
                             TargetData eAddr -> \s -> writeDMem (eval eAddr s) (eval val s) s
                             
                       in (Right "", updateState current : past)
                          
    Set16 target val -> let updateState = case target of
                              TargetPC -> setPC =<< eval val
                        in (Right "", updateState current : past)
                          
  where
    showCurrentInst h = printf "\nPC = 0x%04X\n%s\n"
                        (programCounter (head h))
                        (show (fetchDecode (head h)))
                          
loop :: [AVRState] -> IO [AVRState]
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
repl pmem = (iterateM loop [initialState pmem]) >> return ()