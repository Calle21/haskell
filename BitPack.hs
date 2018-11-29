module BitPack where

import Data.Bits
import Data.Array
import Data.Word

bitPack :: Integral a => Int -> Int -> [a] -> Array Int Word
bitPack size offset elems = let elems' = if offset == 0 then elems
                                         else map (subtract offset) elems
                            in bitPack' elems'
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

bitUnpack :: Integral a => Int -> Int -> Array Int Word -> [a]
bitUnpack size offset arr
  | size < 1  = error "size less than one"
  | otherwise = let elems = unpack 0
                in if offset == 0 then elems
                   else map (+offset) elems
  where
  unpack :: Int -> [a]
  unpack bit
    | storageLeft < size = []
    | otherwise          = getOne 0 size (bit `divMod` wordLength) : unpack (bit + size)
       where
       storageLeft = (fromIntegral $ B.length arr) * wordLength - bit
       getOne :: a -> Int -> (Int,Int) -> a
       getOne accum left (ix,bits)
         | left <= 0 = accum
         | otherwise = let catch    = arr `B.index` fromIntegral ix
                           clean    = mask (wordLength - bits) .&. catch
                           shifted  = (fromIntegral clean :: a) `shift` leftnext
                           leftnext = left - wordLength + bits
                       in getOne (accum .|. shifted) leftnext (ix + 1,0)

 -- Utils

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,length xs - 1) xs

decode :: (Integral a, Integral b) => Int -> a -> (b,a)
decode amount num
  | amount < 1 = error "Amount less then one"
  | otherwise  = let ret = fromIntegral $ num .&. mask amount
                     new = num `shiftR` amount
                 in (ret,new)

hup :: (a -> a) -> [a] -> [a]
hup fn (x:xs) = fn x : xs
hup _  []     = error "Empty list"

mask :: Integral a => Int -> a
mask amount = fromIntegral $ 2 ^ amount - 1

wordLength :: Int
wordLength = rec (maxBound :: Word)
  where
  rec :: Word -> Int
  rec 0 = 0
  rec n = 1 + rec (n `shiftR` 1)

zero = (==0)
