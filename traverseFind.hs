import System.Directory(listDirectory, makeAbsolute)
import Utilities(safeTail)
import Text.Regex.Posix((=~))
import System.FilePath.Posix((</>))
import Control.Monad(forM)

traverseFind :: FilePath -> [String] -> IO [FilePath]
traverseFind start pats = do abstart <- makeAbsolute start
                             wins <- (find abstart . breakdown) `mapM` pats
                             return $ concat wins
  where breakdown ""      = []
        breakdown path    = let (a,b) = break (=='/') path
                            in a : breakdown (safeTail b)
        find :: FilePath -> [String] -> IO [FilePath]
        find place (x:xs) = do contents <- listDirectory place
                               let matches = filter (=~ x) contents
                               winlist <- forM matches $ (`find` xs) . (place </>)
                               return $ concat winlist
        find place _      = return [place]
