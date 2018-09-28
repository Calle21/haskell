module CLang.Indent (indent, Indent(..), getLine, getColumn) where

import Prelude hiding (getLine)
import CLang.Error
import CLang.Lex(Token, RTok)

data Indent = Line Int [LTok]
            | Indent [Indent]
            deriving (Eq, Read, Show)

getLine :: Indent -> Int
getLine (Line ln _) = ln
getLine (Indent xs) = getLine $ head xs

getColumn :: Indent -> Int
getColumn (Line _ ((c,_):_) = c
getColumn (Indent (x:_))   = getColumn x

indent :: [(String, [RTok])] -> [(String, Indent)]
indent files = doIt `map` files
  where
  doIt :: (String, [RTok]) -> (String, Indent)
  doIt (filename, ls) = (filename, fst $ getIndent [] 1 ls)
    where
    getIndent :: [Indent] -> Int -> [RTok] -> (Indent, [RTok])
    getIndent acc level toks@(x:_) =
      case col x `compare` level of
        LT  -> (Indent level $ reverse acc, toks)
        EQ  -> let (l,toks') = getLine [] (line x) toks
               in getIndent (l : acc) level toks'
        GT  -> let (i,toks') = getIndent [] (col x) toks
                   acc'      = i : acc
               in if null toks' || col (head toks') < level
                  then (Indent level $ reverse acc', toks')
                  else getIndent acc' level toks'
    getIndent acc level _ = (Indent level $ reverse acc, [])
    getLine :: [LTok] -> Int -> [RTok] -> (Indent, [RTok])
    getLine acc ln (x:xs)
      | ln == line x  = getLine ((col x, token x) : acc) ln xs
    getLine acc ln xs = (Line ln $ reverse acc, xs)

col, line :: RTok -> Int
col  (c, _, _) = c
line (_, l, _) = l

token :: RTok -> Token
token (_, _, t) = t
