module Main where

import qualified Data.ByteString as B
import Data.Bits
import Data.Word (Word8,  Word16)
import qualified  Data.Vector as V

import Control.Monad

import System.Process (system)
import System.Environment (getArgs)

import AVR.REPL
import AVR.REPL.Expr

-- | Re-interpets a list of Word8 (as in from a ByteString unpack) as a list of Word16
word8to16 :: [Word8] -> [Word16]
word8to16 (a:b:rest) = comb : word8to16 rest
  where
    comb = ((fromIntegral b) `shiftL` 8) + (fromIntegral a)
word8to16 [] = []
word8to16 _ = error "input list must have even number of bytes."

-- | Takes an assembler source frmo stdin and simulates it.
main :: IO()
main = do
  -- assemble the input
  source <- liftM head getArgs
  _ <- system ("cp " ++ source ++ " temp.s")
  _ <- system "avr-as -mmcu=avr5 temp.s -o temp.a"
  _ <- system "avr-ld -mavr5 -Tlinker.x -o temp.elf temp.a"
  _ <- system "avr-objcopy -S -O binary temp.elf temp.bin"
  pmem <- liftM (V.fromList . word8to16 . B.unpack) (B.readFile "temp.bin")
  _ <- system "rm -f temp.s temp.a temp.elf temp.bin"

  repl pmem
