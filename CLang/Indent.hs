module CLang.Indent (indent, Indent(..)) where

import Prelude hiding (getLine)
import CLang.Error
import CLang.Lex(Token)

data Indent = Line Int [(Int, Token)]
            | Indent Int [Indent]
            deriving (Eq, Read, Show)

indent :: String -> [(Int, Int, Token)] -> Indent
indent filename toks = fst $ getIndent [] 1 toks
  where
  getIndent :: [Indent] -> Int -> [(Int, Int, Token)] -> (Indent, [(Int, Int, Token)])
  getIndent acc level toks@(x:_) =
    case level `compare` col x of
      GT  -> (Indent level $ reverse acc, toks)
      EQ  -> let (l,toks') = getLine [] (line x) toks
             in getIndent (l : acc) level toks'
      LT  -> let (i,toks') = getIndent [] (col x) toks
                 acc'      = i : acc
             in if null toks' || level > col (head toks') then (Indent level $ reverse acc', toks')
                else if level == (col $ head toks')
                     then getIndent acc' level toks'
                     else cError "Indent" (col $ head toks') (line $ head toks') filename "Bad indentation"
  getIndent acc level _ = (Indent level $ reverse acc, [])
  getLine :: [(Int, Token)] -> Int -> [(Int, Int, Token)] -> (Indent, [(Int, Int, Token)])
  getLine acc ln (x:xs)
    | ln == line x  = getLine ((col x, token x) : acc) ln xs
  getLine acc ln xs = (Line ln $ reverse acc, xs)

col, line :: (Int, Int, Token) -> Int
col  (c, _, _) = c
line (_, l, _) = l

token :: (Int, Int, Token) -> Token
token (_, _, t) = t
