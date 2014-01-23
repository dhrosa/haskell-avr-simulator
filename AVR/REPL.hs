module AVR.REPL where

import AVR.Decoder (decode, Instruction (NOP))
import AVR.Fetch (fetch)
import AVR.Exec (exec)
import AVR.RegFile (prettyRegFile)
import AVR.AVRState

import Data.Char (toLower)
import Data.List (inits)
import Data.Word (Word16)
import Numeric (readHex)
import Data.Vector ((!))
import qualified Data.Vector as V

import Control.Monad

import System.Console.Readline
import System.Exit
import Text.Printf (printf)
import Text.ParserCombinators.Parsec

data Command = Regs
             | Disassemble
             | Back
             | Step
             | Quit
             | PMem Int Int
             deriving (Eq, Show)

keywords :: [String] -> Parser String 
keywords = choice . map (try . string)

-- | Makes a simple parse that matches the name of a command, or any of its prefixes
parseSimple :: Command -> Parser Command
parseSimple command = keywords (reverse $ tail $ inits phrase) >> eof >> return command
                      <?> phrase ++ " command"
  where phrase = map toLower $ show $ command

parsePMem :: Parser Command
parsePMem = do
  _ <- keywords (reverse $ tail $ inits "pmem")
  _ <- many1 space
  start <- liftM (fst . head . readHex) (many hexDigit)
  _ <- many1 space
  end <- liftM (fst . head . readHex) (many hexDigit)
  return (PMem start end)
  

parseCommand :: Parser Command
parseCommand = choice . map try $ [
  parseSimple Regs,
  parseSimple Disassemble,
  parseSimple Back,
  parseSimple Step,
  parseSimple Quit,
  parsePMem
  ]

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
      pmem = programMemory state
      disassemble = unlines $ V.toList $ V.imap disLine pmem
      disLine :: Int -> Word16 -> String
      disLine i word = let marker = if (fromIntegral i == oldProgramCounter state)
                                    then "> "
                                    else "  "
                       in printf "%s%04X: %s" marker i (show . decode $ word)
                          
      printPMem start end = unlines $ map pMemLine [start..end]
      pMemLine i = printf "%04X: %04X" i $ programMemory state ! i
  
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
            Regs -> putStrLn (prettyRegFile (regFile state) ++ show (sreg state))  >> again
      
            Disassemble -> putStrLn disassemble >> again
      
            Back -> case (back steps) of
              Nothing   -> putStrLn "Cannot backtrack any further." >> again
              Just prev -> simulate prev
        
            Step -> case (forward steps) of
              Nothing -> putStrLn "Cannot step any further." >> again
              Just next -> simulate next
      
            Quit -> putStrLn "Exiting." >> exitSuccess
            
            PMem start end -> putStrLn (printPMem start end) >> again
          )

repl :: ProgramMemory -> IO()
repl pmem = (simulate . toZipper . stepUntilDone . initialState $ pmem) >> return ()