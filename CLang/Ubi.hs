module Nova.Ubi where

import Data.Char (isLower, isUpper)
import Data.List(find, isPrefixOf, partition, sortBy)
import Data.Ord (comparing)
import Data.Word (Word)
import Nova.Error
import Nova.Types
import Prelude hiding (getLine, lex)
import Text.Regex.PCRE((=~))

infixr 2 or
infix  1 match

tags :: String -> Bool
tags s = s =~ "[a-z]+"

specialFiles = ["chain",
                "enum",
                "ops",
                "struct",
                "synonym",
                "tag",
                "type",
                "union",
                "use"]
