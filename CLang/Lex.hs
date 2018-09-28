module CLang.Lex (Token(..), lex) where

import Prelude hiding (lex)
import CLang.Error
import CLang.Syntax
import Text.Regex.PCRE
import Utilities(split)
import Data.List(isPrefixOf)
import Filesystem.Path(filename)
import Data.Char(isUpper, isLower)

data Token = Hash Int
           | Infix String
           | InfixDecl (Either Int Int)
           | Keyword String
           | Name String
           | Nested [String]
           | Opname String
           | Punct Char
           | Special String
           | TChar Char
           | TFloat Float
           | TInt Int
           | TString String
           | Type String
           | Typevar String
           deriving (Eq, Show, Read)

type RTok = (Int, Int, Token) -- Col, Line, Token

lex :: [FilePath] -> IO [(String, [RTok])]
lex paths = do
  ss <- readFile `mapM` paths
  return $ lex `map` zip (filename `map` paths) ss
  where
  lex :: (String, String) -> (String, [RTok])
  lex (filename, s) = (filename, loop 1 1 s)
    where
    loop :: Int -> Int -> String -> [RTok]
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
      countUntil :: (Char -> Bool) -> String -> (Int, String)
      countUntil = loop 0
        where
        loop n f inp
          | null inp     = (n, [])
          | f $ head inp = (n, inp)
          | otherwise    = loop (n + 1) f (tail inp)
      token :: Char -> (Token, Int, String)
      token c = if c `elem` ",()[]{}^" then (Punct c, 1, tail inp)
                else case c of
                  '"'  -> getString "" 1 (tail inp)
                  '\'' -> let (p,n) = inp =~ "^'([^\\\\']|(\\\\([nt\\\\']|x[0-9a-fA-F]{2})))'" :: (Int,Int)
                          in if p == (-1)
                             then lError col line filename "Bad character token"
                             else ((TChar $ read $ take n inp), n, drop n inp)
                  _    -> let (s, inp')                                      = span symChar inp
                              sym | null s                                   = lError col line filename ("Illegal character : " ++ [c])
                                  | keyword s                                = Keyword s
                                  | names s                                  = Name s
                                  | types s                                  = Type s
                                  | s =~ ("^[\\\\" ++ specialChars ++ "]+$") = Opname s
                                  | s =~ "^-?\\d+$"                          = TInt $ read s
                                  | s =~ "^[A-Z]+$"                          = Typevar s
                                  | s =~ "^`[a-z][a-z0-9]*`$"                = Infix $ init $ tail s
                                  | s =~ "^([a-z0-9]+\\.)+[a-z0-9]+$"        = let ss = split '.' s
                                                                               in if all names ss
                                                                                  then Nested ss
                                                                                  else lError col line filename ("Bad nested identifier : " ++ s)
                                  | s =~ "^-?\\d+\\.\\d+$"                   = TFloat $ read s
                                  | s =~ "^[RL]\\d+$"                        = let p = read $ tail s
                                                                               in case head s of
                                                                                    'R'  -> InfixDecl $ Right p
                                                                                    'L'  -> InfixDecl $ Left p
                                  | s =~ "^#\\d+$"                           = Hash $ read $ tail s
                                  | s =~ "^\\*[a-zA-Z]+\\*$"                 = Special s
                                  | otherwise                                = lError col line filename ("Bad token : " ++ s)
                          in (sym, length s, inp')
        where
        names, types :: String -> Bool
        names s = s =~ "^[a-z0-9]*$" && any isLower s
        types s = s =~ "^[A-Z][a-zA-Z0-9]*$" && not (all isUpper s)
        getString :: String -> Int -> String -> (Token, Int, String)
        getString acc n s
          | not (head s `elem` "\n\\\"") = getString (head s : acc) (n + 1) (tail s)
          | s =~ "^\""                   = (TString $ reverse acc, n + 1, tail s)
          | s =~ "^\\\\[nt\\\\\"]"       = getString ((case s !! 1 of
                                                         'n'  -> '\n'
                                                         't'  -> '\t'
                                                         '\\' -> '\\'
                                                         '"'  -> '"') : acc) (n + 2) (drop 2 s)
          | s =~ "^\\\\."                = lError (col + n + 1) line filename ("Illegal escape character in string : " ++ [s !! 1])
          | s =~ "^\\\\?$"               = lError (col + n) line filename "File ended in string literal"
          | s =~ "^\\x0A"                = lError (col + n) line filename "Literal newlines not allowed in string literals. Try \\n instead"
