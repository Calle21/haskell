module CLang.Parse (parse, getCol, getLine) where

import Prelude hiding (getLine)
import CLang.Error
import CLang.Lex(Token, Token'(..))

data Leaf = Symbol String
          | PInt Int
          | PFloat Float
          | PString String
          | PChar Char
          | PBool
          | Unsigned
          | Sub [Leaf]

type ParseTree = [Leaf]

desugar :: [Token] -> ParseTree
desugar toks = undefined
