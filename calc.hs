
 -- A very simple prefix calculator

import Utilities(safeTail)
import Data.List(isPrefixOf)
import Data.Char(isDigit)
import System.IO

main = hSetBuffering stdout NoBuffering >> rec []

rec :: [Integer] -> IO ()
rec stack = do putStr ((show (reverse stack)) ++ ": ")
               line <- getLine
               if line `isPrefixOf` "quit"
                then return ()
                else rec (calc stack (words line))

calc :: [Integer] -> [String] -> [Integer]
calc stack (command:commands)
  | all isDigit command = calc (read command : stack) commands
  | otherwise           = case command of
                   "+"  -> calc (op (+) stack) commands
                   "-"  -> calc (op (-) stack) commands
                   "*"  -> calc (op (*) stack) commands
                   "/"  -> calc (op (flip div) stack) commands
                   "."  -> calc (safeTail stack) commands
                   _    -> calc stack commands
calc stack _ = stack

op :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer]
op f (x:y:xs) = f x y : xs
op _ xs       = xs
