module Coordinates where

import Data.Word (Word16)
import Data.Int (Int32)

newtype Angles = Angles [Word16]

newtype Coordinate = Coordinate [Int32]

data Vector = Vector { origin     :: Coordinate ,
                       conclusion :: Coordinate }

newtype Triangle = [Coordinate]

 -- Functions on vectors

magnitude :: Vector -> Double
magnitude v0 = distance (origin v0) (conclusion v0)

direction :: Vector -> Angles
direction v0 = angles (origin v0) (conclusion v0)

vector = Vector

 -- Functions on two coordinates

distance :: Coordinate -> Coordinate -> Double
distance (Coordinate l0) (Coordinate l1) =
  let [a,rest] = zipWith (-) l0 l1
  in foldl (\d0 d1 -> sqrt $ fromIntegral $ d0 ^ 2 + d1 ^ 2) a rest

angles :: Coordinate -> Coordinate -> Angles
angles (Coordinate l0) (Coordinate l1) =

 -- Functions on one coordinate

depth :: Coordinate -> Int -- The amount of dimensions in a coordinate
depth (Coordinate l) = length l

vectorize :: Coordinate -> Vector
vectorize c0 = Vector (Coordinate $ repeat 0 $ depth c0) c0

 -- Triangulate an unsorted list of coordinates

triangulate :: [Coordinate] -> [Triangle]
triangulate [] = error "No coordinates"
triangulate cs = if length cs <= d
                 then error "Too few coordinates to create any pieces"
                 else let sorted = sortOn (distance $ head cs) cs







              let (p0,rest) = splitAt d cs
                   in rec [p0] rest
  where
  rec :: [Piece] -> [Coordinate] -> (Int,[Piece])
  rec ps (c:cs) = rec (include c ps) cs
    where
    include :: Coordinate -> [Piece] -> [Piece]
    include c ps = 
  rec ps _ = (d,ps)
