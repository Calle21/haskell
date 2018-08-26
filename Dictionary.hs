
 -- This actually took me two days

module Dictionary (makeDictionaries, translate) where

import System.IO
import Utilities(listToArray, String', binarySearch)
import System.Directory(makeAbsolute, withCurrentDirectory, listDirectory)
import Data.Array
import Control.Monad(liftM)
import Data.List(find, sortOn)
import Data.Tuple(swap)

data Dictionary = Dictionary {from       :: String,
                              to         :: String,
                              dictionary :: Array Int (String',String')} deriving (Show, Read, Eq)

dictZip :: [String'] -> [(String',String')]
dictZip (a:b:rest) = (a,b) : dictZip rest
dictZip _          = []

dictUnzip :: [(Dictionary,Dictionary)] -> [Dictionary]
dictUnzip ((a,b):xs) = a : b : dictUnzip xs
dictUnzip []         = []

makeDictionaries :: FilePath -> IO [Dictionary]
makeDictionaries path = do
  names <- listDirectory path
  files <- withCurrentDirectory path $ mapM makeAbsolute names
  dictUnzip `liftM` mapM make (zip files (break' `map` names))
  where
    break' name = let (from',_:to') = break (=='_') name
                   in (from',to')
    make (file,(from',to')) = do
       str <- readFile file
       let words' = words str
           dict   = dictZip (listToArray `map` words')
       return (Dictionary {from = from', to = to', dictionary = listToArray $ sortOn fst dict},
               Dictionary {from = to', to = from', dictionary = listToArray $ sortOn fst (swap `map` dict)})

translate :: [Dictionary] -> String -> String -> String -> Maybe String
translate dicts from' to' word = case find (\dict -> from dict == from' && to dict == to') dicts of
                               Just dict  -> case binarySearch fst (listToArray word) (dictionary dict) of
                                               Just (_,w)  -> Just (elems w)
                                               Nothing     -> Nothing
                               Nothing    -> Nothing
                                   
