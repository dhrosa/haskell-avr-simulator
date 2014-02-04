{-# LANGUAGE GADTs #-}

module AVR.REPL.Expr where

import Data.Word
import Data.Bits
import AVR.AVRState

import Control.Applicative

data Expr a where
  Lit16 :: Word16 -> Expr Word16
  Lit8 :: Word8 -> Expr Word8
  Reg :: Int -> Expr Word8
  
  Add :: (Num a) => Expr a -> Expr a -> Expr a
  Subtract :: (Num a) => Expr a -> Expr a -> Expr a
  
  Low :: (Integral a, Bits a, Integral b) => Expr a -> Expr b
  High :: (Integral a, Bits a, Integral b) => Expr a -> Expr b
  HighExt :: (Integral a, Bits a, Integral b) => Expr a -> Expr b
  
eval :: Expr a -> AVRState -> a
eval (Lit16 n) = const n
eval (Lit8  n) = const n
eval (Reg   n) = getReg (toEnum n)
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