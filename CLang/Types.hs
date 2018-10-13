module Nova.Types where

import Data.Map.Strict (Map)
import Prelude hiding (getLine)

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


data Function = Function (fntype  :: FunctionType,
                          rettype :: [String],
                          pattern :: [Pattern],
                          body    :: Code,
                          locals  :: Setup)
              deriving (Read, Show)


data FunctionType = Action | Pure deriving (Eq, Read, Show, Enum)


type Identifier = String

data Indent = Line Int [Tok]
            | Indent [Indent]
            deriving (Read, Show)

getLine, getColumn :: Indent -> Int
getLine   (Line ln _)    = ln
getLine   (Indent (y:_)) = getLine y
getColumn (Line _ (x:_)  = fst x
getColumn (Indent (y:_)) = getColumn y


type Lex = (Int, Int, Token)

getcl, getln :: Lex -> Int
getcl (c, _, _) = c
getln (_, l, _) = l

gettk :: Lex -> Token
gettk (_, _, t) = t


newtype Local = Local (Identifier, [Overload]) deriving (Read, Show)


newtype Locals = Locals (Map Identifier [Overload])


type MChain = [(String, String)]

type MFix = Map String Fixity

data Module = Module {autotags :: [([String],String)],
                      chains   :: [(String,String)],
                      fixity   :: Map String Fixity,
                      types    :: Map [String] [String],
                      values   :: Map Identifier [Overload]}
              deriving (Read, Show)

type MTag = [([String],String)]

type MTyp = Map [String] [String]

type MVal = Map Identifier [Overload]


data Overload = Overload {annotations :: [String],
                          pattern     :: [Pattern],
                          rettype     :: [String],
                          value       :: Code}
              deriving (Read, Show)


data Pattern = PatS String
             | PatT [String]

getTypeAsTuple :: [Pattern] -> [String]
getTypeAsTuple pat = typeConcat $ filterIt pat
  where
  filterIt ((PatT t):xs) = t : filterIt xs
  filterIt ((PatS _):xs) = filterIt xs
  filterIt []            = []


type PMonad = [Tok] -> Maybe [Tok]


data Prim = Signed Int | Unsigned Word deriving (Eq, Read, Show)

instance Ord Prim where
  compare :: Prim -> Prim -> Ordering
  compare (Signed i0)   (Signed i1)   = compare i0 i1
  compare (Unsigned w0) (Unsigned w1) = compare w0 w1
  compare (Signed i)    (Unsigned w)  = compare i (fromIntegral w)
  compare (Unsigned w)  (Signed i)    = compare (fromIntegral w) i


type Setup = [(String,Module)]


type SpecialParse -> (Setup, FilePath, [Indent]) -> (Setup, [Indent])


type Tok = (Int, Token)

getc :: Tok -> Char
getc (_, t) = getC t

getf :: Tok -> Float
getf (_, t) = getF t

geti :: Tok -> Int
geti (_, t) = getI t

gets :: Tok -> String
gets (_, t) = getS t


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

getC :: Token -> Char
getC (AChar c) = c
getC (Punct c) = c

getF :: Token -> Float
getF (AFloat f) = f

getI :: Token -> Int
getI (AInt i)  = i
getI (HashI i) = i

getS :: Token -> String
getS (AString s)  = s
getS (HashBind s) = s
getS (Name s)     = s
getS (Opname s)   = s
getS (Reserved s) = s
getS (Special s)  = s
getS (Tag s)      = s
getS (Type s)     = s
getS (Vartype s)  = s


type TokP = Tok -> Bool


type Type = [String]

typeConcat :: [Type] -> Type
typeConcat []     = ["(",")"]
typeConcat (x:xs) = foldl conc2 x xs
  where
  conc2 :: Type -> Type -> Type
  conc2 ["(",")"] ys = ys
  conc2 xs ["(",")"] = xs
  conc2 xs ys        = let xs' = if head xs == "(" then init xs else "(" : xs
                           ys' = if head ys == "(" then tail ys else ys ++ [")"]
                       in xs' ++ [","] ++ ys'
