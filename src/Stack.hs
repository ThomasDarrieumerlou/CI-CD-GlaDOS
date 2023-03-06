module Stack (Stack (..), empty, push, pop, toList) where

newtype Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: Stack a -> a -> Stack a
push (Stack as) a = Stack (a:as)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (a:as)) = (Just a, Stack as)

toList :: Stack a -> [a]
toList (Stack as) = as