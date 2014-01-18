module Main where

import AVR.Types
import qualified AVR.StatusReg as S
import qualified AVR.ALU as A

main :: IO()
main = do
  let
    opTypes = enumFrom A.Complement
    val = 0xAA
    ops = map (\t -> (t, A.alu (A.UnaryOp t val S.empty))) opTypes
  putStrLn . unlines . map show $ ops
  let
    a = 0
    b = 1
    op = A.BinaryOp A.AddCarry a b (S.empty {S.carry = True})
  print (op, A.alu op)