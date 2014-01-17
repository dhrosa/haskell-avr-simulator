module Main where

import AVR.Types
import qualified AVR.Types.StatusReg as S

main :: IO()
main = print (S.set (S.empty) 4 True)