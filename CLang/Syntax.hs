module CLang.Syntax (symChar, specialChars, keyword) where

symChar :: Char -> Bool
symChar c = c >= 'a' && c <= 'z' ||
            c >= 'A' && c <= 'Z' ||
            c >= '0' && c <= '9' ||
            c `elem` "_`" ++ specialChars

specialChars = "\\-+*/<>=&|@~%$!.#:"

keywords  = ["where", "has", "=", ">>", ":=", "if", "->", "else", "true", "false", "...", "=>", "is"]
keyword s = s `elem` keywords
