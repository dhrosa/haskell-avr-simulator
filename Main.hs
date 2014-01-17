module Main where

import AVR.Types
import qualified AVR.StatusReg as S
import qualified AVR.ALU as A

main :: IO()
main = print (S.set (S.empty) 4 True)