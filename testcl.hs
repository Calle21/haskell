module Main where

import Prelude hiding (lex)
import CLang.Lex
import CLang.Syntax
import CLang.Error
import CLang.Indent
import CLang.ToString
import System.Environment(getArgs)

main = do
  [name] <- getArgs
  s <- readFile ("CLang/" ++ name)
  let l = lex name s
  let i = indent name l
  putStrLn "Actual file:"
  putStrLn s
  putStrLn "Indent-String:"
  putStrLn (indentString i)
