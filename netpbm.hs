{- Real World Haskell presents a parser for bitmap images in chapter 10.
 - Here's my take on it. A little messy perhaps -}

import qualified Data.ByteString as BS
import Data.Char(isSpace, isDigit)

conv = fromIntegral . fromEnum
testBS = BS.pack $ conv `map` "P5   2  3  255   %&/aja"
testBS' = BS.pack $ conv `map` "P5  2 3  65000   ()//{}kkjjxx"
testBS''= BS.pack $ conv `map` "P4"

gr = parseP5 testBS
gr' = parseP5 testBS'
gr'' = parseP5 testBS''

data BitmapGreyRaw = BitmapGR {height :: Int,
                               width  :: Int,
                               pic    :: BS.ByteString} deriving (Eq, Show)

parseP5 :: BS.ByteString -> Maybe BitmapGreyRaw
parseP5 bit = if BS.take 2 bit == BS.pack (conv `map` "P5")
           then let isSpace'       = isSpace . toEnum . fromIntegral
                    bit2           = BS.dropWhile isSpace' $ BS.drop 2 bit
                    (height',bit3) = readInt' bit2
                    bit4           = BS.dropWhile isSpace' bit3
                    (width',bit5)  = readInt' bit4
                    bit6           = BS.dropWhile isSpace' bit5
                    (max,bit7)     = readInt' bit6
                    bit8           = BS.dropWhile isSpace' bit7
                    pic' | max == 255 = BS.take (width' * height') bit8
                         | otherwise  = convert (width' * height') bit8
               in Just (BitmapGR height' width' pic')
           else Nothing
 where convert num pic' = BS.pack $ rec 0
        where rec i = if i == num then []
                                  else (pic' `BS.index` (i * 2)) : rec (i + 1)

readInt' :: BS.ByteString -> (Int, BS.ByteString)
readInt' bs = let s = gets 0
                  bs' = BS.drop (length s) bs
             in (read s, bs')
 where gets i = let c = toEnum $ fromIntegral (bs `BS.index` i) :: Char
                    ret | isDigit c = c : gets (i + 1)
                        | otherwise = []
               in ret
