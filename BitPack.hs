module BitPack ( BitPack
               , bitPack
               , bitUnpack
               , lenBitPack
               , retrieve ) where

import Data.Bits
import Data.Array
import Data.Word

data BitPack a = BitPack Int Int (Array Int Word)

bitPack :: Integral a => Int -> Int -> [a] -> BitPack a
bitPack size offset elems = let elems' = if offset == 0 then elems
                                         else map (subtract offset) elems
                            in BitPack size offset (bitPack' elems')
  where
  bitPack' :: Integral a => [a] -> Array Int Word
  bitPack' elems
    | all (< 2 ^ size) elems = listToArray $ rec 0 0 elems
    | otherwise              = error "Some element do not fit in given size"
    where
    rec :: Int -> Word -> [a] -> [Word]
    rec bit prev []     = if bit == 0 then [] else [prev]
    rec bit prev (x:xs) = let (prev', packed) = makeOne
                          in packed ++ rec ((bit + size) `mod` wordLength) prev' xs
      where
      makeOne :: (Word, [Word])
      makeOne = let shiftamount = wordLength - size `mod` wordLength - bit
                    shifted     = x `shift` shiftamount
                    xs          = hup (.|. prev) $ hackIt shifted [] (size + shiftamount)
                in (last xs', init xs')
        where
        hackIt :: a -> [Word] -> Int -> [Word]
        hackIt num accum bit | bit <= 0  = accum
                             | otherwise = let (some,next) = decode wordLength num
                                           in hackIt next (some : accum) (bit - wordLength)

bitUnpack :: BitPack a -> [a]
bitUnpack pack@(BitPack size offset arr) = let elems = unpack 0
                                           in if offset == 0 then elems
                                              else map (+offset) elems
  where
  unpack :: Int -> [a]
  unpack ix
    | ix == lenBitPack pack = []
    | otherwise             = retrieve ix pack : unpack (ix + 1)

lenBitPack :: BitPack a -> Int
lenBitPack (BitPack size _ arr) = (lenarray arr * wordlength) `div` size

retrieve :: Int -> BitPack a -> a
retrieve n pack@(BitPack size _ arr)
  | n < 0                = error "Negative index"
  | n >= lenBitPack pack = error "Index out of bounds"
  | otherwise            = rec 0 ((size * n) `divMod` wordlength) size
    where
    rec :: a -> (Int,Int) -> Int -> a
    rec accum (ix,off) left
      | left <= 0 = accum
      | otherwise = let catch    = arr ! ix
                        leftnext = left - wordlength + off
                        shifted  = w `shift` leftnext
                        clean    = mask (wordlength + leftnext - off) .&. shifted
                    in rec (accum .|. clean) (ix + 1,0) leftnext

 -- Utils

decode :: (Integral a, Integral b) => Int -> a -> (b,a)
decode amount num
  | amount < 1 = error "Amount less then one"
  | otherwise  = let ret = fromIntegral $ num .&. mask amount
                     new = num `shiftR` amount
                 in (ret,new)

hup :: (a -> a) -> [a] -> [a]
hup fn (x:xs) = fn x : xs
hup _  []     = error "Empty list"

lenarray :: Array Int a -> Int
lenarray arr = let (a,b) = bounds arr
               in b - a

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,length xs - 1) xs

mask :: Integral a => Int -> a
mask amount = fromIntegral $ 2 ^ amount - 1

wordLength :: Int
wordLength = rec (maxBound :: Word)
  where
  rec :: Word -> Int
  rec 0 = 0
  rec n = 1 + rec (n `shiftR` 1)

zero = (==0)
