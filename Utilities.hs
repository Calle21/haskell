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

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead []    = Nothing

cycleNTimes :: Int -> [a] -> [a]
cycleNTimes n xs
  | n > 0     = xs ++ cycleNTimes (n - 1) xs
  | otherwise = []
