module AVR.REPL.Command
       (
         Command (..),
         parseCommand
       )
       where

import Data.Word (Word8)

import Text.Parsec.String
import Text.Parsec.Char
--import Text.Parsec.Combinator
import Text.Parsec.Prim ((<?>))

import AVR.REPL.Expr
import AVR.REPL.Expr.Parser

data Command = Print8 (Expr Word8)

print8 :: Parser Command
print8 = string "print" >> spaces >> (expr8 >>= return . Print8)
         <?> "print command"

parseCommand :: Parser Command
parseCommand = print8