{-# LANGUAGE GADTs #-}

module AVR.REPL.Expr where

import Data.Word
import Data.Bits
import AVR.AVRState

import Control.Applicative

import Control.Monad
import Numeric (readHex)
import Text.Parsec.String
import Text.Parsec.Expr

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
  
