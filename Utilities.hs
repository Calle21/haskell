module Utilities where

import Data.Char(toUpper, toLower, digitToInt)
import Data.Array

type String' = (Array Int Char)

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

ntimes :: Int -> IO a -> IO ()
ntimes n act 
  | n > 0     = act >> ntimes (n - 1) act
  | otherwise = return ()

listToArray :: [a] -> Array Int a
listToArray l = listArray (0, length l - 1) l

listToArray' :: [a] -> Array Int a
listToArray' l = listArray (1, length l) l

upcaseFirst :: [Char] -> [Char]
upcaseFirst (c:cs) = toUpper c : cs
upcaseFirst []     = []

downcaseFirst :: [Char] -> [Char]
downcaseFirst (c:cs) = toLower c : cs
downcaseFirst []     = []

binarySearch :: (Ord a) => (b -> a) -> a -> Array Int b -> Maybe b
binarySearch fn elt arr = search (bounds arr)
  where
    search (low,hi)
     | low > hi  = Nothing
     | otherwise = let mid   = (low + hi) `div` 2
                       match = arr ! mid
                       elt'  = fn match
                     in case compare elt elt' of
                         LT  -> search (low,mid - 1)
                         EQ  -> Just match
                         GT  -> search (mid + 1, hi)
