module CLang.Syntax (symChar, specialChars, keyword) where

symChar :: Char -> Bool
symChar c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c `elem` "_`" ++ specialChars

specialChars = "\\-+*/<>=&|@~%$!.#:"

keywords  = ["_",
             "=",
             ":=",
             ">>",
             "->",
             "=>",
             "<-",
             "...",
             "catch",
             "else",
             "false",
             "from",
             "goto",
             "has",
             "if",
             "infix",
             "is",
             "the",
             "module",
             "struct",
             "tag",
             "throw",
             "true",
             "type",
             "union",
             "where"]

keyword s = s `elem` keywords
