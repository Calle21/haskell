
  -- From Real World Haskell (ch3)

module Angles (grahamScan) where

import Data.List(sort, sortBy, nub)
import Data.Ord(comparing)

type Point = (Float,Float)
data Direction = LeftTurn | Straight | RightTurn

getDirection :: Point -> Point -> Point -> Direction
getDirection p1 p2 p3 = case compare (crossProduct p1 p2 p3) 0 of
                          GT  -> LeftTurn
                          EQ  -> Straight
                          LT  -> RightTurn

crossProduct :: Point -> Point -> Point -> Float
crossProduct (x1,y1) (x2,y2) (x3,y3) =
  (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1) -- from wikipedia

kOf :: Point -> Point -> Float
kOf (x1,y1) (x2,y2) = (y2 - y1) / (x2 - x1)

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan points = let
                      (p1:ps) = sort (nub points)
                      rest = sortBy (comparing (kOf p1)) ps
                    in rec p1 p1 rest
   where rec :: Point -> Point -> [Point] -> [Point]
         rec start before (middle:nxt@(after:_)) = case getDirection before middle after of
                                                      RightTurn  -> rec start before nxt
                                                      _          -> before : rec start middle nxt
         rec start before [middle] = before : case getDirection before middle start of
                                                      RightTurn  -> []
                                                      _          -> [middle]
         rec _ before _ = [before]
