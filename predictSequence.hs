
 -- Predicts a sequence

predictSequence :: Num a => Char -> [Int] -> [Int]
predictSequence '+' = makeSequence (+) . getDifflist (-) . map fromIntegral
predictSequence '*' = makeSequence (*) . getDifflist (/) . map fromIntegral

makeSequence :: Num a => (Double -> Double -> Double) -> [Double] -> [Int]
makeSequence f difflist = (floor . head) difflist : makeSequence f (processDifflist f difflist)

processDifflist :: (Double -> Double -> Double) -> [Double] -> [Double]
processDifflist f (x:y:xs) = (x `f` y) : processDifflist f (y : xs)
processDifflist _ xs = xs

getDifflist :: (Double -> Double -> Double) -> [Double] -> [Double]
getDifflist f l = reverse $ map head (diff [l])
  where diff ls = if length (head ls) == 1
                 then ls
                 else diff (process (head ls) : ls)
          where process (x:y:xs) = y `f` x : process (y:xs)
                process _        = []
