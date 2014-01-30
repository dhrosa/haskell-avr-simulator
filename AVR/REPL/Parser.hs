module AVR.REPL.Parser
       (
         Command (..),
         parseCommand,
         parse
       )
       where

import Data.Char (toLower)
import Data.List (inits)
import Control.Monad
import Numeric (readHex)

import Text.ParserCombinators.Parsec

data Command = Regs
             | Back
             | Step
             | Quit
             | IORegs
             | PMem Int Int
             | SP
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
  parseSimple Back,
  parseSimple Step,
  parseSimple IORegs,
  parseSimple Quit,
  parsePMem,
  parseSimple SP
  ]
