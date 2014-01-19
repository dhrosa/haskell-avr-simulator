module Main where

import qualified AVR.Decoder as D
import AVR.State
import AVR.Exec
import AVR.RegFile

import qualified Data.ByteString as B
import Data.Bits

import Data.Word (Word8,  Word16)

word8to16 :: [Word8] -> [Word16]
word8to16 (a:b:rest) = comb : word8to16 rest
  where
    comb = ((fromIntegral b) `shiftL` 8) + (fromIntegral a)

word8to16 [] = []

word8to16 _ = error "input list must have even number of bytes."

main :: IO()
main = do
  progmem <- B.getContents
  print . map D.decode . word8to16 . B.unpack $ progmem