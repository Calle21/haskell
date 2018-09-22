module CLang.Parse (parse) where

import Prelude hiding (getLine)
import CLang.Error
import CLang.Lex(Token(..))
import CLang.Indent(Indent(..))

data Leaf = Symbol String
          | PInt Int
          | PFloat Float
          | PString String
          | PChar Char
          | PBool
          | Unsigned
          | Sub [Leaf]

type ParseTree = [Leaf]

getLine :: Indent -> Int
getLine (Line ln _) = ln
getLine (Indent _ is) = getLine $ head is

parseExpr :: String -> [Indent] -> 
parseExpr filename is = let (l:e) = lets `span` is
                        in ["let", parseLets l, parseE e)
  where
  parseLets :: [Indent] -> ParseTree
  parseLets (x:xs) 


lets :: Indent -> Bool
lets (Line _ toks) = case snd `map` toks of
                       (Type _:Name _:Keyword "=":_:_) -> True
                       _                               -> False
lets _             = False

getSymbolTable :: Indent -> [SymbolTable] -> SymbolTable
getSymbolTable i = 

desugar :: Indent -> [SymbolTable] -> ParseTree
desugar toks = undefined
