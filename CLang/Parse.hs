module CLang.Parse (parse) where

import CLang.Lex(Token)
import qualified CLang.Env as E
import CLang.Env(getEnv)
import CLang.Parse.Match(match)
import CLang.Parse.Header(parseHeader)
import CLang.Parse.Body(parseBody)
import Data.Foldable(foldlM)
import Control.Monad(>=>)

data Prim = Signed Int | Unsigned Int deriving (Eq, Ord, Read, Show)

type Type = [String]

data Code = Array      Type   [Code]  -- Type [Code]
          | Call       Code   Code    -- Function Code
          | Catch      String         -- String
          | Define     String Code    -- String Function
          | Defspecial String Code    -- String Function
          | From       String String  -- Modname Identifier
          | Link       String Code    -- String Code
          | Primitive  Prim   Integer -- Type Int
          | Seq        Code   Code    -- Code Code
          | The        Type   Code    -- Type Code
          | Throw      String Code    -- String Code
          | Tuple      [Code]         -- [Code]
          deriving (Eq, Ord, Read, Show)

parse :: [(String, Indent)] -> IO (String, [Environment])
parse files = do
  let usefile = case "use" `lookup` files of
                  Just x  -> x
                  Nothing -> error "Couldn't find use file"
  (name, setup) <- parseUse usefile
  let (setup', files') = foldl parseHeader (setup, []) $ filter (\x -> fst x /= "use") files
  return (name, foldl parseBody setup' files')
  where
  parseUse :: Indent -> IO (String, [Environment])
  parseUse (Indent [(Line 1 xs)]) | xs `match` listOf1 (Keyword "->") isEnd (one isType) =
    let ss = getS `map` (fst $ getList1 isEnd xs)
    in do envs <- getEnv `mapM` tail ss
          return (head ss, E.empty : envs)
  parseUse _ = error "Couldn't parse use file"
    where
    getS :: Token -> String
    getS (Type s) = s
