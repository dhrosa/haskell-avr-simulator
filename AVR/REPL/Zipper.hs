module AVR.REPL.Zipper
       (
         Zipper,
         toZipper,
         forward1,
         forward,
         back1,
         back,
         current,
         updateCurrent,
         updateFuture
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

-- | Replaces the current value in the zipper with a new one
updateCurrent :: a -> Zipper a -> Zipper a
updateCurrent val (_:as, bs) = (val:as, bs)
updateCurrent _ z = z

-- | Updates every point past the current point
updateFuture  :: [a] -> Zipper a -> Zipper a
updateFuture future (_, bs) = (future, bs)
