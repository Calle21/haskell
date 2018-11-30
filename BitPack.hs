module BitPack ( BitPack(..)
               , bitPack
               , bitUnpack
               , lenBitPack
               , retrieve
               , retrieveBool
               , sizeBitPack ) where

import Data.Array
import Data.Bits
import Data.Word

data BitPack a = BitPack Int (Integer -> a) Int (Array Int Word)

bitPack :: (Integer -> a) -> Int -> [Integer] -> BitPack a
bitPack conv size elems = BitPack (length elems) conv size (bitPack' elems)
  where
  bitPack' :: [Integer] -> Array Int Word
  bitPack' elems
    | any (< 0)        elems = error "Some element is negative"
    | all (< 2 ^ size) elems = listToArray $ rec 0 0 elems
    | otherwise              = error "Some element do not fit in given size"
    where
    rec :: Int -> Word -> [Integer] -> [Word]
    rec bit prev []     = if bit == 0 then [] else [prev]
    rec bit prev (x:xs) = let (prev', packed) = makeOne
                          in packed ++ rec ((bit + size) `mod` wordlength) prev' xs
      where
      makeOne :: (Word, [Word])
      makeOne = let shiftamount = wordlength - size `mod` wordlength - bit
                    shifted     = x `shift` shiftamount
                    ys          = hup (.|. prev) $ hackIt shifted [] (size + shiftamount)
                in (last ys, init ys)
        where
        hackIt :: Integer -> [Word] -> Int -> [Word]
        hackIt num accum bit | bit <= 0  = accum
                             | otherwise = let (lowest,rest) = decode wordlength num
                                           in hackIt rest (lowest : accum) (bit - wordlength)

bitUnpack :: BitPack a -> [a]
bitUnpack pack = unpack 0
  where
  unpack ix
    | ix == lenBitPack pack = []
    | otherwise             = retrieve ix pack : unpack (ix + 1)

lenBitPack :: BitPack a -> Int
lenBitPack (BitPack len _ _ _) = len

retrieve :: Int -> BitPack a -> a
retrieve n pack@(BitPack len conv size arr)
  | n < 0     = error "Negative index"
  | n >= len  = error "Index out of bounds"
  | otherwise = conv $ rec 0 ((size * n) `divMod` wordlength) size
    where
    rec :: Integer -> (Int,Int) -> Int -> Integer
    rec accum (ix,off) left
      | left <= 0 = accum
      | otherwise = let catch    = arr ! ix
                        clean    = mask (wordlength - off) .&. catch
                        shifted  = clean `shift` leftnext
                        leftnext = left - wordlength + off
                    in rec (accum .|. fromIntegral shifted) (ix + 1,0) leftnext

retrieveBool :: Int -> BitPack Bool -> Bool
retrieveBool n pack@(BitPack len _ _ arr)
  | n < 0     = error "Negative index"
  | n >= len  = error "Index out of bounds"
  | otherwise = let (ix,bit) = n `divMod` wordlength
                    catch    = arr ! ix
                    shifted  = catch `shiftR` (wordlength - 1 - fromIntegral bit)
                in odd shifted

sizeBitPack :: BitPack a -> Int
sizeBitPack (BitPack _ _ size _) = size

 -- Utils

decode :: (Integral a, Integral b, Bits a) => Int -> a -> (b,a)
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

wordlength :: Int
wordlength = rec (maxBound :: Word)
  where
  rec :: Word -> Int
  rec 0 = 0
  rec n = 1 + rec (n `shiftR` 1)

zero = (==0)
