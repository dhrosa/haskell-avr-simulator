{-# LANGUAGE GADTs #-}

module AVR.REPL.Expr where

import Data.Word
import Data.Bits
import AVR.AVRState

import Control.Applicative

import Control.Monad
import Numeric (readHex)

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Expr
import Text.Parsec.Prim ((<?>), try)

data Expr a where
  -- Basic Units
  Lit8 :: (Integral a) => a -> Expr Word8
  Lit16 :: (Integral a) => a -> Expr Word16
  Reg :: RegNum -> Expr Word8
  IOReg :: IOAddress -> Expr Word8
  PC :: Expr Word16
  
  -- Memory Access
  PMem :: (Integral a) => Expr a -> Expr Word16
  DMem :: (Integral a) => Expr a -> Expr Word8
  
  -- Unary Operators
  Low :: (Integral a, Bits a) => Expr a -> Expr Word8
  High :: (Integral a, Bits a) => Expr a -> Expr Word8
  HighExt :: (Integral a, Bits a) => Expr a -> Expr Word8
  
  -- Binary operators
  Add :: (Num a) => Expr a -> Expr a -> Expr a
  Subtract :: (Num a) => Expr a -> Expr a -> Expr a
  
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
  guard (num <= 0xFF)
  return $ Lit8 num
  <?> "8-bit literal"
  
-- | Parses a 16-bit literal
lit16 :: Parser (Expr Word16)
lit16 = do
  num <- literal :: Parser Int
  guard (num <= 0xFFFF)
  return $ Lit16 num

-- | Parses a register
reg8 :: Parser (Expr Word8)
reg8 = do
  _ <- oneOf "Rr"
  num <- read <$> many1 digit
  return (Reg (toEnum num))
  <?> "register"

-- | Parses an 8-bit expression
expr8 :: Parser (Expr Word8)
expr8 = buildExpressionParser opTable term
  where
    binOp name func assoc = Infix (string name >> return func) assoc
    prefixOp name func = Prefix (string name >> return func)
--    postfixOp name func = Postfix (string name >> return func)
                      
    opTable = [
      [prefixOp "D@" DMem],
      [binOp "+" Add AssocLeft, binOp "-" Subtract AssocLeft]
      ]
              
    term = foldl1 (<|>) [
      lit8,
      reg8
      ]
           
-- | Evaluates the value of an expression in the context of the current processor state.
eval :: Expr a -> AVRState -> a
eval (Lit8 n) = const (fromIntegral n)
eval (Lit16 n) = const (fromIntegral n)
eval (Reg   n) = getReg n
eval (IOReg n) = readIOReg n
eval PC        = programCounter

eval (PMem  a) = do
  addr <- fromIntegral <$> eval a
  fromIntegral . readPMem16 addr
eval (DMem  a) = do
  addr <- fromIntegral <$> eval a
  fromIntegral . readDMem addr

eval (Add a b) = (+) <$> eval a <*> eval b
eval (Subtract a b) = (-) <$> eval a <*> eval b
eval (Low a) = do
  val <- eval a
  return $ fromIntegral $ 0xFF .&. val
eval (High a) = do
  val <- eval a
  return $ fromIntegral $ 0xFF .&. (val `shiftR` 8)
eval (HighExt a) = do
  val <- eval a
  return $ fromIntegral $ 0xFF .&. (val `shiftR` 16)
  
