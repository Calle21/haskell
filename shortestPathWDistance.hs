
 -- Extension of the shortest path program (I think this is a little too hard for me)

import Data.List(intercalate)

data Node = Node {name       :: Char
                 ,x          :: Float
                 ,y          :: Float
                 ,neighbours :: [Char]} deriving (Show)

type Network = [Node]

findPath :: String -> Network -> IO ()
findPath [start,end] net = case netOk of
                        Left s -> putStrLn s
                        _      -> putStrLn (rec [([], start, 0.0)] Nothing)
 where rec queue win
        | null queue = case win of
                        Nothing    -> "The nodes you specified do not seem to be connected."
                        Just (h,d) -> showIt (reverse h, d)
        | otherwise  = let (history, nxt, distance):rest = queue
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
                         where rec (x:xs) c
                                | c == name x = x
                                | otherwise   = rec xs c
       showIt (history, distance) = intercalate " -> " (map (\a -> [a]) history) ++ " ; " ++ show distance
       getDistance :: Node -> Node -> Float
       getDistance (Node _ x1 y1 _) (Node _ x2 y2 _) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
       netOk
        | null net                  = Left "Net empty"
        | not (consecutive 'a' net) = Left "Nodes must be named consecutively, starting with the character \'a\'"
        | not neighboursOk          = Left "Some node is referring to a neighbour that do not exist."
        | otherwise                 = Right ()
        where consecutive _ [] = True
              consecutive start (x:xs) = if start == name x then consecutive (succ start) xs
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
