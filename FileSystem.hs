module FileSystem (newFileSystem, index, FileSystem.lookup, delete) where

import qualified Data.Map.Strict as M -- update, empty
import Data.Map.Strict(Map, insertWith, update)
import Data.List(intersect, foldl')
import qualified Data.List as L -- delete
import Utilities(maybeList, listMaybe, dfoldl')
import Data.Array(Array)

type Tag = Array Int Char

type FileSystem = Map Tag [File]

data File = File {fname :: String,
                  ftags :: [Tag],
                  ffn   :: Int} deriving (Eq, Ord, Read, Show)

newFileSystem :: FileSystem
newFileSystem = M.empty

index :: File -> FileSystem -> FileSystem
index file fs = foldl' (\l r -> insertWith (++) r [file] l) fs (ftags file)

lookup :: FileSystem -> [Tag] -> [File]
lookup fs tags = dfoldl' intersect [] ((maybeList . (`M.lookup` fs)) `map` tags)

delete :: FileSystem -> File -> FileSystem
delete fs file = foldl' (flip $ update (\x -> listMaybe $ L.delete file x)) fs (ftags file)

 {- Instead of the hierarchy based filesystems we've all gotten used to, I think a filesystem
  - based on tags would be a lot easier to navigate. Here's some code -}
