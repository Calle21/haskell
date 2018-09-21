module CLang.ToString where

import CLang.Lex(Token(..))
import CLang.Indent(Indent(..))
import Data.List(intercalate)

tokenString :: Token -> String
tokenString t = case t of
                  Hash n     -> '#' : show n
                  Infix s    -> '`' : s ++ "`"
                  Keyword s  -> s
                  Name s     -> s
                  Nested ss  -> "." `intercalate` ss
                  Opname s   -> s
                  Punct c    -> [c]
                  TChar c    -> show c
                  TFloat f   -> show f
                  TInt n     -> show n
                  TString s  -> show s
                  Type s     -> s
                  Typevar s  -> s
                  Underscore -> "_"

indentString :: Indent -> String
indentString (Line (_, ts)) = pred (fst $ head ts) `replicate` ' ' ++ " " `intercalate` ((tokenString . snd) `map` ts) ++ "\n"
indentString (Indent (1, is)) = separate $ indentString `map` is
  where
  separate :: [String] -> String
  separate (x:y:xs) = x ++ (if head y /= ' ' then "\n" else "") ++ separate (y:xs)
  separate (x:_)    = x
indentString (Indent (_, is)) = indentString `concatMap` is
