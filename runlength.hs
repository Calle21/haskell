compress :: [Bool] -> (Bool, [Int])
compress []    = (True, [])
compress (h:t) = (h, rec 1 h t)
 where rec n _ [] = [n]
       rec n p (h:t) | p == h    = rec (n + 1) p t
                     | otherwise = n : rec 1 h t

decompress :: (Bool, [Int]) -> [Bool]
decompress (begin, lst) = rec begin lst
 where rec _ []    = []
       rec b (h:t) = make h b ++ rec (not b) t
                      where make 0 _ = []
                            make n b = b : make (n - 1) b

longlist = [True, True, True, False, True, False, False, False, True, False, False, True, True, False, False, False,
            True, True, True, False, False, True, True, True, True, True, True, True, True, False, True, False, False,
            False, False, True, False]

complist = compress longlist
decomplist = decompress complist
success = longlist == decomplist
