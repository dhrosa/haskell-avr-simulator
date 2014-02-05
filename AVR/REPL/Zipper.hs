module AVR.REPL.Zipper
       (
         Zipper,
         toZipper,
         forward1,
         forward,
         back1,
         back,
         current
       )
       where

import Control.Monad

type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper x = (x, [])

forward1 :: Zipper a -> Maybe (Zipper a)
forward1 ([_], _) = Nothing
forward1 (a:as, bs) = Just (as, a:bs)
forward1 _ = Nothing

forward :: Int -> Zipper a -> Maybe (Zipper a)
forward n = foldl (>=>) return (replicate n forward1)

back1 ::  Zipper a -> Maybe (Zipper a)
back1 (_, []) = Nothing
back1 (as, b:bs) = Just (b:as, bs) 

back :: Int -> Zipper a -> Maybe (Zipper a)
back n = foldl (>=>) return (replicate n back1)

current :: Zipper a -> a
current (a:_, _) = a
current _  = error "Cannot take current value of empty zipper."