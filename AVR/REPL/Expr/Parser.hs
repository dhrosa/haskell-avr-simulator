module AVR.REPL.Expr.Parser where

import AVR.REPL.Expr
       
import Data.Word (Word8, Word16)
  
import Control.Applicative hiding (optional)
import Control.Monad
import Numeric (readHex)

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim ((<?>), try)

-- | Parses a decimal value
decimal :: (Integral a, Read a) => Parser a
decimal = read <$> many1 digit
          <?> "decimal value"

-- | Parses a hexadecimal value
hex :: (Integral a, Read a) => Parser a
hex = do
  _ <- string "0x"
  [(val, "")] <- readHex <$> many1 hexDigit
  return val
  <?> "hexadecimal value"
  
-- | Parses an integral literal value
literal :: (Integral a, Read a) => Parser a
literal = try hex <|> decimal
          <?> "literal"
  
-- | Parses an 8-bit literal
lit8 :: Parser (Expr Word8)
lit8 = do
  num <- literal :: Parser Int
  guard (num <= 0xFF) <?> "8-bit literal between 0 and 0xFF"
  return $ Lit8 num
  <?> "8-bit literal"
  
-- | Parses a 16-bit literal
lit16 :: Parser (Expr Word16)
lit16 = do
  num <- literal :: Parser Int
  guard (num <= 0xFFFF) <?> "16-bit literal between 0 and 0xFFFF"
  return $ Lit16 num
  <?> "16-bit literal"

-- | Parses a register
reg8 :: Parser (Expr Word8)
reg8 = do
  _ <- oneOf "Rr"
  num <- read <$> many1 digit
  guard (num <= 31) <?> "reg number between 0 and 31"
  return (Reg (toEnum num))
  <?> "register"

ioReg :: Parser (Expr Word8)
ioReg = do
  _ <- choice [string "IO", string "io"]
  addr <- literal
  return (IOReg addr)

-- | Parses an 8-bit expression
expr8 :: Parser (Expr Word8)
expr8 = buildExpressionParser opTable term
  where
    binOp name func = Infix (spaces >> string name >> spaces >> return func)
    prefixOp name func = Prefix (string name >> spaces >> return func)
--    postfixOp name func = Postfix (string name >> return func)
                      
    opTable = [
      [prefixOp "D@" DMem],
      [binOp "+" Add AssocLeft, binOp "-" Subtract AssocLeft]
      ]
              
    term = foldl1 (<|>) [
      between (string "(") (string ")") expr8,
      lit8,
      reg8,
      to8
      ]
           
pc :: Parser (Expr Word16)
pc = choice [string "pc", string "PC"] >> return PC
           
expr16 :: Parser (Expr Word16)
expr16 = buildExpressionParser opTable term
  where
    binOp name func = Infix (spaces >> string name >> spaces >> return func)
    prefixOp name func = Prefix (string name >> spaces >> return func)

    opTable = [
       [prefixOp "P@" PMem],
       [binOp "+" Add AssocLeft, binOp "-" Subtract AssocLeft]
      ]
              
    term = foldl1 (<|>) [
      lit16,
      pc
      ]
           
to8 :: Parser (Expr Word8)
to8 = do
  op <- choice ops
  operand <- between (string "(") (string ")") expr16
  return (op operand)
  <?> "16-bit to 8-bit converter"
  where
    conversionOp name func = string name >> return func
    ops = map (uncurry conversionOp) [
      ("LO", Low),
      ("HI", High),
      ("EX", HighExt)
      ]
