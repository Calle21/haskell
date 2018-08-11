
 -- Extension of the shortest path program (I think this is a little too hard for me)

module ShortestPathWDistance () where

import Data.List(intercalate)

data Node = Node {name       :: Char
                 ,x          :: Float
                 ,y          :: Float
                 ,neighbours :: [Char]} deriving (Show)

type Network = [Node]

find :: String -> Network -> IO ()
find [start,end] net = do ok <- netOk
                          if ok then putStrLn (rec [([], start, 0.0)] Nothing)
                                else putStrLn "Computation aborted."
                          return ()
 where rec queue win
        | null queue = case win of
                        Nothing    -> "The nodes you specified do not seem to be connected."
                        Just (h,d) -> showIt (reverse h, d)
        | otherwise  = let first@(history, nxt, distance):rest = queue
                           history' = nxt : history
                           distance' = case history of
                                            []    -> 0.0
                                            (h:_) -> distance + getDistance (getNode nxt) (getNode h)
        in if nxt `elem` history || case win of Nothing    -> False
                                                Just (_,d) -> distance' > d
          then rec rest win -- Eliminate this path
          else if nxt == end && case win of Nothing    -> True
                                            Just (_,d) -> distance' < d
            then rec rest (Just (history', distance')) -- New win path
            else rec (rest ++ makeNew history' distance' (neighbours (getNode nxt))) win -- Continue searching this path
                  where makeNew h d neighbours'
                         | null neighbours' = []
                         | otherwise        = (h, head neighbours', d) : makeNew h d (tail neighbours')
                        getNode = rec net
                         where rec (f:r) c
                                | c == name f = f
                                | otherwise   = rec r c
       showIt (history, distance) = intercalate " -> " (map (\a -> [a]) history) ++ " ; " ++ show distance
       getDistance :: Node -> Node -> Float
       getDistance (Node _ x1 y1 _) (Node _ x2 y2 _) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
       netOk :: IO Bool
       netOk
        | null net = do putStrLn "Net empty."
                        return False
        | not (consecutive 'a' net) = do putStrLn "Nodes must be named consecutively, starting with the character \'a\'"
                                         return False
        | not neighboursOk = do putStrLn "Some node is referring to a neighbour that do not exist."
                                return False
        | otherwise = return True
        where consecutive _ [] = True
              consecutive start (f:r) = if start == name f then consecutive (succ start) r
                                                           else False
              neighboursOk = let topChar = name (last net)
                          in not (any (>topChar) (concat (map neighbours net)))

netA = [Node 'a' 0.0 0.0 "bc",
        Node 'b' 1.0 1.0 "c",
        Node 'c' 1.0 0.0 "d",
        Node 'd' 2.0 0.0""]

netB = [Node 'a' 0.0 0.0 "bc",
        Node 'b' 1.0 1.0 "ac",
        Node 'c' 1.0 0.0 "de",
        Node 'd' 2.0 0.0 "be",
        Node 'e' 2.0 1.0 "a"]

netC = [Node 'b' 1.0 1.0 "ab"]
netD = [Node 'a' 0.0 0.0 "ab"]
