{-# LANGUAGE FlexibleInstances #-}

 -- Chapter 5 of RWH presents a JSON-library. Here's a JSON parser

module JSONParse (fileToJSON, JSON, JSONVal(..)) where

import Data.Array
import Data.List(intercalate, isPrefixOf)
import Data.List.Extra(for)
import Data.Char(isSpace, isDigit)
import System.IO(Handle, stdout, hPutStr)

class JSON a where
  toJSON :: a -> JSONVal
  fromJSON :: JSONVal -> a

instance JSON Double where
  toJSON = JSONNum
  fromJSON (JSONNum d) = d

instance JSON Bool where
  toJSON = JSONBool
  fromJSON (JSONBool b) = b

instance JSON String where
  toJSON = JSONString
  fromJSON (JSONString s) = s

instance JSON (Array Int JSONVal) where
  toJSON = JSONArray
  fromJSON (JSONArray a) = a

instance JSON [(String, JSONVal)] where
  toJSON = JSONObject
  fromJSON (JSONObject o) = o

showJSON :: JSONVal -> String
showJSON (JSONNum d)      = show d
showJSON (JSONString s)   = show s
showJSON (JSONBool True)  = "true"
showJSON (JSONBool False) = "false"
showJSON JSONNull         = "null"
showJSON (JSONArray a)    = "[" ++ intercalate ", " (for (elems a) showJSON) ++ "]"
showJSON (JSONObject o)   = "{" ++ intercalate ", " (map (\(s,j) -> show s ++ ": " ++ showJSON j)
                                                         o) ++ "}"

showJSON' :: (JSON a) => a -> String
showJSON' = showJSON . toJSON

data JSONVal = JSONNum Double
             | JSONString String
             | JSONBool Bool
             | JSONNull
             | JSONArray (Array Int JSONVal)
             | JSONObject [(String, JSONVal)] deriving (Eq, Show, Read, Ord)

parseJSON :: String -> [JSONVal]
parseJSON s = case parseJSON' s of
                Just (j,rest)  -> j : parseJSON rest
                Nothing        -> []

isNumChar :: Char -> Bool
isNumChar c = isDigit c || c == '.'

parseJSON' :: String -> Maybe (JSONVal, String)
parseJSON' ('"':s) = let (s',_:rest) = break (=='"') s
                     in Just (JSONString s', rest)
parseJSON' s@(c:cs)
 | isSpace c              = parseJSON' cs
 | isDigit c              = let (s',rest) = span isNumChar s
                            in Just (JSONNum (read s'), rest)
 | "true" `isPrefixOf` s  = Just (JSONBool True, drop 4 s)
 | "false" `isPrefixOf` s = Just (JSONBool False, drop 5 s)
 | "null" `isPrefixOf` s  = Just (JSONNull, drop 4 s)
parseJSON' ('[':as) = parseJSONArray [] as
parseJSON' ('{':os) = parseJSONObject [] os
parseJSON' []       = Nothing
parseJSON' s        = error s

parseJSONArray :: [JSONVal] -> String -> Maybe (JSONVal, String)
parseJSONArray acc s@(c:cs)
 | isSpace c = parseJSONArray acc cs
 | c == ']'  = Just (JSONArray (listArray (0, length acc - 1) (reverse acc)), cs)
 | otherwise = case parseJSON' s of
                Just (j, rest)  -> parseJSONArray (j : acc) (toComma ']' rest)
                Nothing         -> error "End in array"

parseJSONObject :: [(String, JSONVal)] -> String -> Maybe (JSONVal, String)
parseJSONObject acc s@(c:cs)
 | isSpace c    = parseJSONObject acc cs
 | c == '}'     = Just (JSONObject acc, cs)
 | otherwise    = case parseJSON' s of
                   Just (JSONString s, rest)  -> case parseJSON' (toColon rest) of
                                                  Just (j,rest')  -> parseJSONObject ((s,j) : acc) (toComma '}' rest')
                                                  Nothing         -> error "End in object"
                   Just _                     -> error "Expected string (in object)"
                   Nothing                    -> error "End in object"

toComma :: Char -> String -> String
toComma end s@(c:cs)
 | isSpace c = toComma end cs
 | c == ','  = cs
 | c == end  = s
 | otherwise = error "Parse fail when looking for a comma"

toColon :: String -> String
toColon (c:cs)
 | isSpace c = toColon cs
 | c == ':'  = cs
 | otherwise = error "Expected a colon"

fileToJSON :: FilePath -> IO [JSONVal]
fileToJSON path = readFile path >>= return . parseJSON

hPrintJSON :: Handle -> [JSONVal] -> IO ()
hPrintJSON h vals = hPutStr h $ intercalate "\n\n" (showJSON `map` vals)

printJSON :: [JSONVal] -> IO ()
printJSON = hPrintJSON stdout
