module Main where

import AVR.Types
import qualified AVR.StatusReg as S
import qualified AVR.ALU as A

main :: IO()
main = do
  let
    opTypes = enumFromTo A.Complement A.Set
    val = 0xAA
    ops = map (\t -> (t, A.alu (A.UnaryOp t val S.empty))) opTypes
  putStrLn . unlines . map show $ ops