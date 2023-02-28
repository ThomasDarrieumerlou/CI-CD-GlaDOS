module Stack (Stack (..), empty, push, pop) where

data Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push a (Stack as) = Stack (a:as)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (a:as)) = (Just a, Stack as)