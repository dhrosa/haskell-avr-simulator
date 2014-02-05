module AVR.REPL.Command
       (
         Command (..),
         parseCommand
       )
       where

import Data.Word (Word8, Word16)

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim ((<?>), (<|>), try)

import AVR.REPL.Expr
import AVR.REPL.Expr.Parser

data Command = Print8 (Expr Word8)
             | Print16 (Expr Word16)
             | Step Int
             | Back Int

print8 :: Parser Command
print8 = string "print" >> spaces >> (expr8 >>= return . Print8)
         <?> "print command"

print16 :: Parser Command
print16 = string "print" >> spaces >> (expr16 >>= return . Print16)
         <?> "print command"
    
step :: Parser Command
step = do
  _ <- string "step"
  spaces
  val <- optionMaybe literal
  return $ Step (maybe 1 id val)

back :: Parser Command
back = do
  _ <- string "back"
  spaces
  val <- optionMaybe literal
  return $ Back (maybe 1 id val)

parseCommand :: Parser Command
parseCommand = foldl1 (<|>) [
  try print16,
  print8,
  step,
  back
  ]