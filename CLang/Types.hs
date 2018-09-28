module CLang.Types where

type LTok = (Int, Token)
type RTok = (Int, Int, Token)
type PMonad = [LTok] -> Maybe [LTok]
type PPredicate = LTok -> Bool
type TypeDescriptor = [String]
