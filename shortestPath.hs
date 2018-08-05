
 -- Not from RWH

module ShortestPath () where

data Node = Node {name       :: Char
                 ,neighbours :: [Char]} deriving (Show)

find :: String -> [Node] -> IO ()
find [start,end] net = do ok <- let topChar :: Char
                                    topChar = toEnum (fromEnum 'a' + (length net) - 1)
                                in netOk
                          if ok then putStrLn (rec [([], start)])
                                else putStrLn "Computation aborted."
                          return ()
 where rec queue
        | null queue = "The nodes you specified does not seem to be connected."
        | otherwise  = let first@(history, nxt):rest = queue
        in if nxt `elem` history
          then rec rest
          else if nxt == end
           then show (reverse (nxt : history))
           else rec (rest ++ makeNew (nxt : history) (neighbours (getNode nxt)))
                  where makeNew history' neighbours'
                         | null neighbours' = []
                         | otherwise        = (history', head neighbours') : makeNew history' (tail neighbours')
                        getNode = rec net
                         where rec (f:r) c
                                | c == name f = f
                                | otherwise   = rec r c
       netOk :: IO Bool
       netOk
        | null net = do putStrLn "Net empty."
                        return False
        | not (consecutive 'a' net) = do putStrLn "Nodes must be named consecutively, starting with the character \'a\'"
                                         return False
        | not neighboursOk = do putStrLn "Some node is referring to a neighbour that do not exist."
                                return False
        | otherwise = do putStrLn "Net ok"
                         return True
        where consecutive _ [] = True
              consecutive start (f:r) = if start == name f then consecutive (succ start) r
                                                           else False
              neighboursOk = let topChar = name (last net)
                          in not (any (>topChar) (concat (map neighbours net)))

netA = [Node 'a' "bc",
        Node 'b' "c",
        Node 'c' "d",
        Node 'd' ""]

netB = [Node 'a' "bc",
        Node 'b' "ac",
        Node 'c' "de",
        Node 'd' "be",
        Node 'e' "a"]

netC = [Node 'b' "ab"]
netD = [Node 'a' "ab"]
