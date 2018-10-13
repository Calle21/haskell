module Parse.Struct (parseStruct) where

import CLang.Types
import CLang.Parse.Util

parseStruct :: Setup -> [Indent] -> Setup
parseStruct (m:ms) ((Line ln (_:xs)):ys) =
  if xs `match` aType >=> isEnd
  then let typename      = snd `map` xs
           (fields, ys') = ys `follows` 4
           (typed,accessors) | null fields = pError (ln + 1) filename "No fields given for struct definition"
                             | otherwise   =
                                  let (types,names) = unzip $ readField `map` fields
                                      desc = (toLower `map` typename) : "(" : "," `intersperse` types ++ [")"]
                                  in (desc,names)
             where
             readField :: Indent -> [String]
             readField y = let (ln, xs) = theLine y filename
                           in if xs `match` aType >=> one isName >=> isEnd
                              then (snd `map` init xs, snd $ last xs)
                              else pError ln filename "Bad struct field"
       in undefined
  else pError ln filename "Bad struct name (it should be a type)"
