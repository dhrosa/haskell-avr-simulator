module AVR.REPL.Expr.Parser where

import AVR.REPL.Expr
import AVR.AVRState (RegNum)

import Data.Word (Word8, Word16)
  
import Control.Applicative hiding (optional)
import Control.Monad
import Numeric (readHex)

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim ((<?>), try)

-- | Wraps another parser in parentheses
parens :: Parser a -> Parser a
parens = between left right
  where
    left = string "(" >> spaces
    right = spaces >> string ")"

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
  
regNum :: Parser RegNum
regNum = do
  _ <- oneOf "Rr"
  num <- read <$> many1 digit
  guard (num <= 31) <?> "reg number between 0 and 31"
  return (toEnum num)

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
reg8 = regNum >>= (return . Reg)

ioReg :: Parser (Expr Word8)
ioReg = choice [string "IO", string "io"] >> decimal >>= (return . IOReg)

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
      parens expr8,
      lit8,
      reg8,
      to8
      ]
           
pc :: Parser (Expr Word16)
pc = choice [string "pc", string "PC"] >> return PC
     
-- | Parses a 16-bit expression
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
      parens expr16,
      to16,
      lit16,
      pc
      ]
           
-- | Parses an function that converts a 16-bit expression to an 8-bit expression
to8 :: Parser (Expr Word8)
to8 = do
  op <- choice ops
  operand <- parens expr16
  return (op operand)
  <?> "16-bit to 8-bit converter"
  where
    conversionOp name func = string name >> return func
    ops = map (uncurry conversionOp) [
      ("LO", Low),
      ("HI", High),
      ("EX", HighExt)
      ]

-- | Parses a function that converts an 8-bit expression to a 16-bit expression
to16 :: Parser (Expr Word16)
to16 = do
  op <- choice ops
  operand <- parens expr8
  return (op operand)
  <?> "8-bit to 16-bit converter"
  where
    conversionOp name func = string name >> return func
    ops = map (uncurry conversionOp) [
      ("ZE", ZeroExtend),
      ("SE", SignExtend)
      ]