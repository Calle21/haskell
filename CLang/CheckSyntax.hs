module CLang.CheckSyntax (checkSyntax) where

import CLang.Lex(Token(Type, Typevar))

data Code = Assign Code Code             -- Call Code
          | Call String Code             -- String Tuple
          | CBool Bool
          | CChar Char
          | CFloat Float
          | CInt Int
          | CString String
          | Declare Code Code            -- Type Code
          | Define Code String Code Code -- Tuple Name Tuple Code
          | Module [String]
          | Name String
          | Seq Code Code
          | Tuple [Code]
          | Type [Token]
          deriving (Read, Show)

getLine :: Indent -> Int
getLine (Line ln _) = ln
getLine (Indent _ is) = getLine $ head is

getColumn :: Indent -> Int
getColumn (Line _ ((c,_):_) = c
getColumn (Indent c _)      = c

checkSyntax :: String -> Indent -> [Code]
checkSyntax filename (Indent 1 (x:xs)) =

data Environment = Environment {infixs    :: Map String (Either Int Int),
                                types     :: Map [String] ...,
                                functions :: Map String [...]}
                 deriving (Read, Show)
