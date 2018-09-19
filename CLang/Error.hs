module CLang.Error (cError) where

import Utilities(format)

cError :: String -> Int -> Int -> String -> String -> a
cError part col line filename message = error $ format "%0 error on line %1, column %2\n  %3 (%4)\n" [part, show line, show col, message, filename]
