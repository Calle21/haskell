module CLang.Indent (indent) where

import CLang.Types
import CLang.Error

indent :: [(String, [LTok])] -> [(String, Indent)]
indent files = doIt `map` files
  where
  doIt :: (String, [LTok]) -> (String, Indent)
  doIt (filename, ls) = (filename, fst $ getIndent [] 1 ls)
    where
    getIndent :: [Indent] -> Int -> [LTok] -> (Indent, [LTok])
    getIndent acc level toks@(x:_) =
      case getcl x `compare` level of
        LT  -> return (acc, toks)
        EQ  -> let (l,toks') = getLine [] (getln x) 0 toks
               in getIndent (l : acc) level toks'
        GT  -> let (i,toks') = getIndent [] (getcl x) toks
                   acc'      = i : acc
               in if null toks' || getcl (head toks') < level
                  then return (acc', toks')
                  else getIndent acc' level toks'
    getIndent acc _     _ = return (acc, [])
    return :: ([Indent], [LTok]) -> (Indent, [LTok])
    return (acc,toks) = (Indent $ reverse acc, toks)
    getLine :: [Tok] -> Int -> Int -> [LTok] -> (Indent, [LTok])
    getLine acc ln end (x:xs)
           | ln == getln x = if gettk x == Keyword "\\"
                            then getLine ((getcl x, Punct '(') : acc) ln (end + 1) xs
                            else getLine ((getcl x, gettk x) : acc) ln end xs
    getLine acc ln end xs = (Line ln $ reverse (npunct end acc), xs)
      where
      npunct :: Int -> [Tok] -> [Tok]
      npunct 0 toks = toks
      npunct n ts = let c = getColumn $ head ts
                    in npunct (n - 1) $ (ln, c + 1, Punct ')') : toks
