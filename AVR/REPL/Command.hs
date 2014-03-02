{-# LANGUAGE GADTs #-}

module AVR.REPL.Command
       (
         Command (..),
         Target8 (..),
         Target16 (..),
         parseCommand
       )
       where

import Data.Word (Word8, Word16)
import Data.Maybe (fromMaybe)

import Control.Monad (liftM)

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim ((<?>), (<|>), try)

import AVR.AVRState
import AVR.REPL.Expr
import AVR.REPL.Expr.Parser (expr8, expr16, literal, regNum)

data Target8 = TargetReg RegNum
             | TargetPCL
             | TargetPCH
             | TargetIO IOAddress
             | TargetData (Expr Word16)

data Target16 = TargetPC

data Command = Inst
             | Regs
             | Print8 (Expr Word8)
             | Print16 (Expr Word16)
             | Step Int
             | Back Int
             | Set8 Target8 (Expr Word8)
             | Set16 Target16 (Expr Word16)

inst :: Parser Command
inst = string "inst" >> return Inst

regs :: Parser Command
regs = string "regs" >> return Regs

print8 :: Parser Command
print8 = string "print" >> spaces >> liftM Print8 expr8
         <?> "print command"

print16 :: Parser Command
print16 = string "print" >> spaces >> liftM Print16 expr16
         <?> "print command"
    
step :: Parser Command
step = do
  _ <- string "step"
  spaces
  val <- optionMaybe literal
  return $ Step (fromMaybe 1 val)

back :: Parser Command
back = do
  _ <- string "back"
  spaces
  val <- optionMaybe literal
  return $ Back (fromMaybe 1 val)

targetReg :: Parser Target8
targetReg = liftM TargetReg regNum

targetPCL :: Parser Target8
targetPCL = choice [string "PCL", string "pcl"] >> return TargetPCL

targetPCH :: Parser Target8
targetPCH = choice [string "PCH", string "pch"] >> return TargetPCH

targetIO :: Parser Target8
targetIO = liftM TargetIO literal

targetData ::  Parser Target8
targetData = string "D@" >> liftM TargetData expr16

target8 :: Parser Target8
target8 = foldl1 (<|>) $ map try [
  targetReg,
  targetPCL,
  targetPCH,
  targetIO,
  targetData
  ]

set8 :: Parser Command
set8 = do
  target <- target8
  spaces
  _ <- string "="
  spaces
  val <- expr8
  return (Set8 target val)

targetPC :: Parser Target16
targetPC = choice [string "PC", string "pc"] >> return TargetPC

target16 :: Parser Target16
target16 = foldl1 (<|>) $ map try [
  targetPC
  ]

set16 :: Parser Command
set16 = do
  target <- target16
  spaces
  _  <- string "="
  spaces
  val <- expr16
  return (Set16 target val)

parseCommand :: Parser Command
parseCommand = foldl1 (<|>) $ map try [
  inst,
  regs,
  print16,
  print8,
  step,
  back,
  set8,
  set16
  ]
