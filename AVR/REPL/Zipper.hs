module AVR.REPL.Zipper
       (
         Zipper,
         toZipper,
         forward,
         back,
         current
       )
       where

type Zipper a = ([a], [a])

toZipper :: [a] -> Zipper a
toZipper x = (x, [])

forward :: Zipper a -> Maybe (Zipper a)
forward ([_], _) = Nothing
forward (a:as, bs) = Just (as, a:bs)
forward _ = Nothing

back ::  Zipper a -> Maybe (Zipper a)
back (_, []) = Nothing
back (as, b:bs) = Just (b:as, bs) 

current :: Zipper a -> a
current (a:_, _) = a
current _  = error "Cannot take current value of empty zipper."