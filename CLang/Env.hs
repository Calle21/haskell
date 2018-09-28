module CLang.Env (Environment(..), getEnv, empty) where

import qualified Data.Map as M
import Data.Map(Map)
import CLang.Parse(Code)
import System.Directory(listDirectory, makeAbsolute)

type TypeDescriptor = [String]

data Environment = Environment {infixs    :: Map String (Either Int Int),
                                types     :: Map String TypeDescriptor,
                                functions :: Map String Code}
                 deriving (Read, Show)


modules :: IO [(String, Environment)]
modules = do
  names <- listDirectory ".modules"
  paths <- makeAbsolute `mapM` names
  ss    <- readFile `mapM` paths
  envs  <- read `mapM` ss
  return (names `zip` envs)

getEnv :: String -> IO Environment
getEnv s = do mods <- modules
              case s `lookup` mods of
                Just env -> return env
                Nothing  -> error ("Couldn't find module : " ++ s)

empty :: Environment
empty = Environment M.empty M.empty M.empty
