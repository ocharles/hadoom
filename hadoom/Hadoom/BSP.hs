{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hadoom.BSP where

import BasePrelude hiding (lefts, rights, lines)
import Hadoom.Geometry
import Linear

data BSP a
  = Split (LineSegment a)
          (BSP a)
          (BSP a)
  | Empty
  deriving (Eq,Ord,Read,Show)

buildBsp :: (Epsilon a, Floating a, Fractional a, Num a, Ord a, Show a) => [LineSegment a] -> BSP a
buildBsp [] = Empty
buildBsp (split:lines) =
  let splitLine l =
        let startSide = lineSide split (start l)
            endSide = lineSide split (end l)
        in case lineLineIntersection l split of
             Nothing ->
               case startSide of
                 LeftSide -> [Left l]
                 RightSide -> [Right l]
                 _ ->
                   case endSide of
                     LeftSide -> [Left l]
                     RightSide -> [Right l]
                     _ -> [Left l] -- The lines are coincident and non-overlapping. We'll arbitrarily bias to the left
             Just p ->
               case startSide of
                 LeftSide ->
                   [Left (LineSegment (start l) p)
                   ,Right (LineSegment p (end l))]
                 RightSide ->
                   [Right (LineSegment (start l) p)
                   ,Left (LineSegment p (end l))]
                 OnLine ->
                   case lineSide split (end l) of
                     LeftSide -> [Left l]
                     RightSide -> [Right l]
                     _ ->
                       error ("buildBsp: Cannot split coincident lines: " ++
                              show l ++ show split)
      approximatesPoint (LineSegment a b) = nearZero (norm (a - b))
      splitLines = filter (not . (either approximatesPoint approximatesPoint)) (concatMap splitLine lines)
      (lefts,rights) = partitionEithers splitLines
  in Split split
           (buildBsp lefts)
           (buildBsp rights)

printIndented :: Show a => BSP a -> String
printIndented = go 0
  where go n Empty = replicate n ' ' <> ".\n"
        go n (Split s l r) = replicate n ' ' <> show s <> "\n" <> go (succ n) l <> go (succ n) r

circleIntersects :: (Epsilon a, Floating a, Ord a) => BSP a -> Circle a -> Bool
circleIntersects Empty _ = False
circleIntersects (Split split l r) c
  | circleLineSegmentIntersection c split = True
  | otherwise = case lineSide split (circleCenter c) of
                  LeftSide -> circleIntersects l c
                  RightSide -> circleIntersects r c
                  OnLine -> True
