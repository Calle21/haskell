module CLang.Parse.Header (parseHeader) where

import CLang.Parse.Util
import CLang.Types

parseHeader :: ([Environment], [(String, Indent)] -> (String, Indent) -> ([Environment], [(String, Indent)])
parseHeader (setup, acc) (filename, Indent ys) = loop setup ys
  where
  loop :: [Environment] -> [Indent] -> ([Environment, [(String, Indent)])
  loop setup ((Line ln xs):ys) | xs `match` one headerStart >=> notEnd =
    case snd (head xs) of
      Keyword "Infix"  -> let colNr        = fst (xs !! 1)
                              (lines, ys') = (Line ln $ tail xs, ys) `followed` colNr
                              setup'       = foldl updateInfix setup lines
                                where
                                updateInfix :: [Environment] -> Indent -> [Environment]
                                updateInfix (Environment t0 t1 t2:es) y =
                                  let (ln, xs) = theLine y filename
                                  in if xs `match` listof1 (Punct ',') (one $ is (Keyword "=>")) (one isfname) >=> one isInfixDecl >=> isEnd
                                     then let (fnames, xs') = getlist1 (one $ is (Keyword "=>")) xs
                                              decl          = (\InfixDecl e -> e) . snd . head $ xs'
                                              t0'           = foldl (insertIt decl) t0 (getfname `map` fnames)
                                          in (Environment t0' t1 t2) : es
                                     else pError ln filename "Couldn't parse infix declaration"
                                  where
                                  insertIt :: Either Int Int -> Map String (Either Int Int) -> String -> Map String (Either Int Int)
                                  insertIt decl map name = insert name decl map
                          in loop setup' ys'
      Keyword "struct" -> if tail xs `match` one isType >=> oneOrNone isVartype
                          then let typename      = snd `map` tail xs
                                   (fields, ys') = ys `follows` 4
                                   typed | null fields = pError (ln + 1) filename "Couldn't find any fields for struct definition"
                                         | otherwise   = (toLower `map` typename) : "(" : "," `intersperse` (readField `map` fields) ++ [")"]
                                     where
                                     readField :: Indent -> [String]
                                     readField x = let (ln, xs) = theLine x filename
                                                   in if xs `match` aType >=> aName >=> isEnd -- Vartype
                                                      then snd `map` init xs
                                                      else pError ln filename "Couldn't parse struct field"
                               in undefined
                          else pError ln filename "Couldn't parse struct name"
      Keyword "union"  -> if tail xs `match` aType >=> is (Keyword "=") >=> aType >=> is (Punct ',') >=> listof1 (Punct ',') isEnd aType
                          then let (name, _:desc) = break (is (Keyword "=")) tail xs
                               in undefined
                          else pError ln filename "Couldn't parse union declaration"
      Keyword "type"   -> if tail xs `match` one isType >=> oneOrNone isVartype >=> one (is Keyword "=") >=> oneOrMore isTypeLang
                          then let (thetype, _:desc) = (is Keyword "=") `break` (tail xs)
                               in 
                          else pError ln filename "Couldn't parse type declaration"
  loop setup ys = (setup, Indent ys : acc)
    where
    headerStart :: PPredicate
    headerStart (_, (Keyword s)) = s `elem` ["type", "union", "struct", "infix"]
    headerStart _                = False

