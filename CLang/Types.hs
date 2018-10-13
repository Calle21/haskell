module Nova.Types where

import Data.Map.Strict (Map)
import Prelude hiding (getLine)

 -- Lex

data Token = AChar Char
           | AFloat Float
           | AInt Int
           | AString String
           | HashI Int
           | HashBind String
           | Name String
           | Opname String
           | Punct Char
           | Reserved String
           | Special String
           | Tag String
           | Type String
           | Vartype String
           deriving (Show, Read)

type Lex = (Int, Int, Token)

getcl, getln :: Lex -> Int
getcl (c, _, _) = c
getln (_, l, _) = l

gettk :: Lex -> Token
gettk (_, _, t) = t

 -- Parse

type Tok = (Int, Token)

data Indent = Line Int [Tok]
            | Indent [Indent]
            deriving (Read, Show)

getLine, getColumn :: Indent -> Int
getLine   (Line ln _)    = ln
getLine   (Indent (y:_)) = getLine y
getColumn (Line _ (x:_)  = fst x
getColumn (Indent (y:_)) = getColumn y

type PMonad = [Tok] -> Maybe [Tok]
type TokP = Tok -> Bool

 -- Interpret

data Prim = Signed Int | Unsigned Word deriving (Eq, Read, Show)

instance Ord Prim where
  compare :: Prim -> Prim -> Ordering
  compare (Signed i0)   (Signed i1)   = compare i0 i1
  compare (Unsigned w0) (Unsigned w1) = compare w0 w1
  compare (Signed i)    (Unsigned w)  = compare i (fromIntegral w)
  compare (Unsigned w)  (Signed i)    = compare (fromIntegral w) i

data Pattern = PatT [String]
             | PatS String

data FunctionType = Action | Pure deriving (Eq, Read, Show, Enum)

data Function = Function (fntype  :: FunctionType,
                          rettype :: [String],
                          pattern :: [Pattern],
                          body    :: Code,
                          locals  :: Setup)
              deriving (Read, Show)

data Code = Array      [String] [Code]
          | Callop     String Code
          | Catch      String
          | Defspecial String Code
          | From       String String
          | Let        Environment Code
          | Name       String
          | Pattern    [Code]
          | Primitive  Prim Integer
          | The        [String] Code
          | Throw      String Code
          | Tuple      [Code]
          deriving (Eq, Ord, Read, Show)

 -- Modules

data Fixity = Infixl Int | Infixr Int | Prefix | Postfix deriving (Read, Show)

win :: (Fixity, Fixity) -> Either () ()
win (Postfix, _) = Left ()
win (_, Postfix) = Right ()
win (Prefix, _)  = Left ()
win (_, Prefix)  = Right ()
win (left,right) = let s0 = getStrength left
                       s1 = getStrength right
                   in if s0 > s1 then Left ()
                      else if s1 > s0 then Right ()
                           else case (left,right) of
                                  (Infixr _, Infixr _) -> Right ()
                                  _                    -> Left ()
  where
  getStrength :: Fixity -> Int
  getStrength (Infixl s) = s
  getStrength (Infixr s) = s

type MTyp = Map [String] [String]
type MFix = Map String Fixity
type MPat = [Pattern]

data Module = Module {types    :: Map [String] [String],
                      fixity   :: Map String Fixity,
                      patterns :: [Pattern],
                      chains   :: [(String,String)]}
              deriving (Read, Show)

type Setup = [(String,Module)]

 -- Compile

 -- Intermediate

 -- Code generator
