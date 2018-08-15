import Data.List(intercalate)

data Node = Node {name       :: Char
                 ,neighbours :: [Char]} deriving (Show)

findPath :: String -> [Node] -> IO ()
findPath [start,end] net = case netOk of
                        Left s  -> putStrLn s
                        Right _ -> putStrLn (rec [([], start)])
 where rec queue
        | null queue = "The nodes you specified do not seem to be connected."
        | otherwise  = let (history, nxt):rest = queue
        in if nxt `elem` history
          then rec rest
          else if nxt == end
           then showIt (reverse (nxt : history))
           else rec (rest ++ makeNew (nxt : history) (neighbours (getNode nxt)))
                  where makeNew history' neighbours'
                         | null neighbours' = []
                         | otherwise        = (history', head neighbours') : makeNew history' (tail neighbours')
                        getNode = rec net
                         where rec (x:xs) c
                                | c == name x = x
                                | otherwise   = rec xs c
                        showIt = intercalate " -> " . map (\a -> [a])
       netOk
        | null net                  = Left "Net empty"
        | not (consecutive 'a' net) = Left "Nodes must be named consecutively, starting with the character \'a\'"
        | not neighboursOk          = Left "Some node is referring to a neighbour that does not exist."
        | otherwise                 = Right ()
        where consecutive _ [] = True
              consecutive start (x:xs) = if start == name x then consecutive (succ start) xs
                                                            else False
              neighboursOk = let topChar = name (last net) -- Must be preceded by consecutive
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
