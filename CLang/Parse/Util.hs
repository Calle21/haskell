module CLang.Parse.Match where

import CLang.Types

 -- Match

infix 1 match

match :: [LTok] -> PMonad -> Bool
match xs m = isJust $ m xs

 -- Monads

 -- Combinators

one :: PPredicate -> PMonad
one p (x:xs) | p x = Just xs
one _ _            = Nothing

oneOrMore :: PPredicate -> PMonad
oneOrMore f (x:xs) | f x = zeroOrMore f xs
oneOrMore _ _ = Nothing

zeroOrMore :: PPredicate -> PMonad
zeroOrMore _ [] = Just []
zeroOrMore f xs | f $ head xs = loop $ tail xs
                | otherwise   = Just xs

zeroOrOne :: PPredicate -> PMonad
zeroOrOne _ [] = Just []
zeroOrOne f xs | f $ head xs = Just $ tail xs
               | otherwise   = Just xs

exactly :: Int -> PPredicate -> PMonad
exactly 0 _ xs           = xs
exactly n f (x:xs) | f x = exactly (n - 1) f xs
exactly _ _ _            = Nothing

listof :: Token -> PMonad -> PMonad -> PMonad
listof sep end cont xs = case end xs of
                           Just xs' -> Just xs'
                           Nothing  -> listof1 sep end cont xs

listof1 :: Token -> PMonad -> PMonad  -> PMonad
listof1 sep end cont xs = case cont xs of
                            Just xs' -> case one sep $ xs' of
                                          Just xs'' -> listof1 sep end cont xs''
                                          Nothing   -> end xs'
                            Nothing  -> Nothing

infixr 2 or

or :: PPredicate -> PPredicate -> PPredicate
or f0 f1 = (\x -> f0 x || f1 x)

is :: Token -> PPredicate
is t = \(_, x) -> x == t

 -- Token predicates

isStartParen :: PPredicate
isStartParen (_, Punct '(') = True
isStartParen _              = False

isEndParen :: PPredicate
isEndParen (_, Punct ')') = True
isEndParen _              = False

isName :: PPredicate
isName (_, Name _) = True
isNAme _           = False

isComma :: PPredicate
isComma (_, Punct ',') = True
isComma _              = False

isModule :: PPredicate
isModule (_, Name "module") = True
isModule _                  = False

isType :: PPredicate
isType (_, Type _) = True
isType _           = False

isVartype :: PPredicate
isVartype (_, Vartype _) = True
isVartype _              = False

isfname :: PPredicate
isfname (_, Name _)   = True
isfname (_, Opname _) = True
isfname _             = False

isTypeLang :: PPredicate
isTypeLang (_,t) = extract t
  where
  extract (Type _)    = True
  extract (Keyword s) = s `elem` ["signed", "unsigned"]
  extract (Opname s)  = s `elem` ["|"]
  extract (Punct c)   = c `elem` ['(', ')', ',']
  extract (Name _)    = True
  extract (TInt _)    = True

 -- Compound

isEnd :: PMonad
isEnd [] = Just []
isEnd _  = Nothing

notEnd :: PMonad
notEnd [] = Nothing
notEnd xs = Just xs

plist :: PMonad
plist = one isStartParen >=> listof (Punct ',') (one (is (Punct ')'))) (aType >=> one isName)

aType :: PMonad
aType = oneOrMore (isType `or` isVartype)

definition :: PMonad
definition ts = aType >=> one isName >=> plist `or` (one (is (Keyword "=")) >=> notEnd)

clist -> PMonad
clist ts = one isStartParen >=> cyclic (expr >=> one isComma

 -- Miscellaneous

theLine :: Indent -> String -> (Int, [LTok])
theLine (Line ln xs) _       = (ln, xs)
theLine (Indent ys) filename = pError (getLine $ head ys) filename "Unexpected indentation"

getList :: Token -> PMonad -> [LTok] -> ([[Token]], [LTok])
getList sep end xs = loop acc xs
  where
  loop :: [[Token]] -> [LTok] -> ([[Token]], [LTok])
  loop acc xs = case end xs of
                  Just xs' -> (reverse acc, xs')
                  Nothing  -> let (toks, _:xs') = span (\x -> snd x /= sep) xs
                              in loop (map snd toks : acc) xs'

getList1 :: PMonad -> [LTok] -> ([Token], [LTok])
getList1 end xs = loop acc xs
  where
  loop :: [Token] -> [LTok] -> ([Token], [LTok])
  loop acc xs = case end xs of
                  Just xs' -> (reverse acc, xs')
                  Nothing  -> loop (snd (head xs) : acc) (drop 2 xs)

followed :: (Indent, [Indent]) -> Int -> ([Indent], [Indent])
followed (previous, follow) level = case follow of
                                      (Indent ys):_ -> if getColumn ys == level then (previous : ys, tail follow)
                                                       else ([previous], follow)
                                      _             -> ([previous], follow)


follows :: [Indent] -> Int -> ([Indent], [Indent])
follows follow level = case follow of
                         (Indent ys):_ -> if getColumn ys == level then (ys, tail follow)
                                          else ([], follow)
                         _             -> ([], follow)

 -- Extract

getfname :: Token -> String
getfname (Name s)   = s
getfname (Opname s) = s
