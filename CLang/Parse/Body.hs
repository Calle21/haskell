module CLang.Parse.Body (parseBody) where

parseBody :: [Environment] -> (String, Indent) -> [Environment]
parseBody (env, acc) (filename, file, env') =

data ParenType = TPLE Bool
               | TYPE Bool
               | RECC Bool
               | CBNM Bool

