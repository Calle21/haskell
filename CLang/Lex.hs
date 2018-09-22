module CLang.Lex where

import Prelude hiding (lex)
import CLang.Error
import CLang.Syntax
import Text.Regex.PCRE
import Utilities(countUntil, split)
import Data.List(isPrefixOf)

data Token = Hash Int
           | Infix String
           | InfixDecl (Either Int Int)
           | Keyword String
           | Name String
           | Nested [String]
           | Opname String
           | Punct Char
           | TChar Char
           | TFloat Float
           | TInt Int
           | TString String
           | Type String
           | Typevar String
           | Underscore
           deriving (Eq, Show, Read)

lex :: String -> String -> [(Int, Int, Token)]
lex filename = loop 1 1
  where
  loop :: Int -> Int -> String -> [(Int, Int, Token)]
  loop col line inp
    | null inp  = []
    | otherwise = case head inp of
                    ' '  -> let (n, inp') = countUntil (/=' ') inp
                            in loop (col + n) line inp'
                    '\n' -> let (n, inp') = countUntil (/='\n') inp
                            in loop 1 (line + n) inp'
                    c    -> if "\\\\" `isPrefixOf` inp then loop col line $ dropWhile (/='\n') inp
                            else let (tok, len, inp') = token c
                                 in (col, line, tok) : loop (col + len) line inp'
    where
    token :: Char -> (Token, Int, String)
    token c = if c `elem` ",()[]{}^" then (Punct c, 1, tail inp)
              else case c of
                '"'  -> getString "" 1 (tail inp)
                '\'' -> let (p,n) = inp =~ "^'([^\\\\']|(\\\\([nt\\\\']|x[0-9a-fA-F]{2})))'" :: (Int,Int)
                        in if p == (-1)
                           then cError "Lex" col line filename "Bad character token"
                           else ((TChar $ read $ take n inp), n, drop n inp)
                _    -> let (s, inp')                                      = span symChar inp
                            sym | null s                                   = cError "Lex" col line filename ("Illegal character : " ++ [c])
                                | keyword s                                = Keyword s
                                | s =~ "^[a-z]+$"                          = Name s
                                | s =~ "^[A-Z][a-z]+$"                     = Type s
                                | s =~ ("^[\\\\" ++ specialChars ++ "]+$") = Opname s
                                | s =~ "^-?\\d+$"                          = TInt $ read s
                                | s =~ "^`[a-z]+`$"                        = Infix $ init $ tail s
                                | s =~ "^([a-z]*\\.[a-z]+)+$"              = if head s == '.' then cError "Lex" col line filename "Didn't expect a dot"
                                                                             else Nested $ split '.' s
                                | s =~ "^-?\\d+\\.\\d+$"                   = TFloat $ read s
                                | s =~ "^[A-Z]+$"                          = Typevar s
                                | s =~ "^[RL]\\d+$"                        = let p = read $ tail s
                                                                             in case head s of
                                                                                  'R'  -> InfixDecl $ Right p
                                                                                  'L'  -> InfixDecl $ Left p
                                | s =~ "^#\\d+$"                           = Hash $ read $ tail s
                                | s =~ "^_$"                               = Underscore
                                | otherwise                                = cError "Lex" col line filename ("Bad token : " ++ s)
                        in (sym, length s, inp')
      where
      getString :: String -> Int -> String -> (Token, Int, String)
      getString acc n s
        | not (head s `elem` "\n\\\"") = getString (head s : acc) (n + 1) (tail s)
        | s =~ "^\""                   = (TString $ reverse acc, n + 1, tail s)
        | s =~ "^\\\\[nt\\\\\"]"       = getString ((case s !! 1 of
                                                       'n'  -> '\n'
                                                       't'  -> '\t'
                                                       '\\' -> '\\'
                                                       '"'  -> '"') : acc) (n + 2) (drop 2 s)
        | s =~ "^\\\\."                = cError "Lex" (col + n + 1) line filename ("Illegal escape character in string : " ++ [s !! 1])
        | s =~ "^\\\\?$"               = cError "Lex" (col + n) line filename "File ended in string literal"
        | s =~ "^\\x0A"                = cError "Lex" (col + n) line filename "Literal newlines not allowed in string literals. Try \\n instead"
