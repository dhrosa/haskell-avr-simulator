module Main where

import qualified AVR.RegFile as R
import qualified AVR.StatusReg as S
import qualified AVR.Decoder as D
import qualified AVR.ALU as A

import qualified Data.ByteString as B
import Data.Bits

import Data.Word (Word8,  Word16)

word8to16 :: [Word8] -> [Word16]
word8to16 (a:b:rest) = comb : word8to16 rest
  where
    comb = ((fromIntegral b) `shiftL` 8) + (fromIntegral a)

word8to16 [] = []

main :: IO()
main = do
  progmem <- B.getContents
  print . map D.decode . word8to16 . B.unpack $ progmem