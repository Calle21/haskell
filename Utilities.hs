module Utilities where

list :: a -> [a]
list x = [x]

member :: (Eq a) => a -> [a] -> [a]
member x m@(y:ys)
  | x == y    = m
  | otherwise = member x ys
member _ [] = []

safeTail :: [a] -> [a]
safeTail (_:xs) = xs
safeTail []     = []

cycleNTimes :: Int -> [a] -> [a]
cycleNTimes n xs
  | n > 0     = xs ++ cycleNTimes (n - 1) xs
  | otherwise = []

mapWI :: (a -> Int -> b) -> [a] -> [b]
mapWI = rec 0
 where rec i f (x:xs) = f x i : rec (i + 1) f xs
       rec _ _ []     = []

ntimes :: Int -> IO a -> IO a
ntimes n act 
  | n > 1     = act >> ntimes (n - 1) act
  | otherwise = act

