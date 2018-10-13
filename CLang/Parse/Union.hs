module CLang.Parse.Union (parseUnion) where

import CLang.Parse.Util
import CLang.Types

parseUnion :: Setup -> [Indent] -> (Setup, [Indent])
parseUnion (m:ms) ((Line ln (x:xs)):ys) =
  if xs `match` aType >=> one (is (Keyword "=")) >=> listof2 (Punct ',') isEnd aType
  then let (name, _:desc) = (is (Keyword "=")) `break` xs
           name'          = gets `map` name
           typed          = trep `map` desc
       in (m {types = insert name' typed (types m)} : ms, ys)
  else pError ln filename "Couldn't parse union declaration"
