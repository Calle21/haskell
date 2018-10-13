module Parse.Ops (parseOps) where

import CLang.Types

parseOps :: Setup -> Indent -> Setup
parseOps setup (Line ln (x:xs)) =
  case snd x of
    Keyword "infixr"  -> case one isInt xs of
                           Just xs' -> makeIt xs' $ Infixr $ getp $ head xs
                           Nothing  -> pError ln "ops" "Expected int after infixr"
    Keyword "infixl"  -> case one isInt xs of
                           Just xs' -> makeIt xs' $ Infixl $ getp $ head xs
                           Nothing  -> pError ln "ops" "Expected int after infixl"
    Keyword "prefix"  -> makeIt xs Prefix
    Keyword "postfix" -> makeIt xs Postfix
    _                 -> pError ln "ops" "Oops.."
  where
  makeIt :: Fixity -> Setup
  makeIt fix = case xs of
                 (p:ops) -> case p of
                              (_, TInt p') -> let ops' = map getSS ops
                                              in if null ops' then pError ln "ops" "Must supply at least one operator here"
                                                 else foldl (insertIt fix p') setup ops
                              _            -> pError ln "ops" "Expected an int"
                 _       -> pError ln "ops" "Hmm..."
    where
    getSS :: (Int, Token) -> String
    getSS (_, Opname s) = s
    getSS _             = pError ln "ops" "That was not an operator name"

insertIt :: Setup -> String -> Setup
insertIt fix p (m:ms) name =
