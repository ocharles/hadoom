{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hadoom.BSP (BSP(..), buildBSP, circleIntersects) where

import BasePrelude hiding (left, lefts, right, rights, lines)
import Hadoom.Geometry
import Linear
import Linear.Affine
import Data.List.NonEmpty (NonEmpty(..))

-- | A binary space partition tree, where each node contains 1 or more
-- coincident line segments (along the separating line), and a smaller BSP tree
-- of line segments to the left, and line segments to the right.
data BSP a
  = Split (NonEmpty (LineSegment (Point V2 a)))
          (BSP a)
          (BSP a)
  | Empty
  deriving (Eq,Ord,Read,Show)

-- | An internal structure to keep track of how a given line segment would
-- partition other lines.
data Split a =
  MkSplit {lefts :: [LineSegment (Point V2 a)]
          ,rights :: [LineSegment (Point V2 a)]
          ,coincidents :: [LineSegment (Point V2 a)]}
  deriving (Eq,Ord,Show)

left, right, coincident :: LineSegment (Point V2 a) -> Split a
left l = mempty { lefts = [l] }
right l = mempty { rights = [l] }
coincident l = mempty { coincidents = [l] }

filterSplit :: (LineSegment (Point V2 a) -> Bool) -> Split a -> Split a
filterSplit f (MkSplit x y z) =
  MkSplit (filter f x)
          (filter f y)
          (filter f z)

instance Monoid (Split a) where
  mempty = MkSplit mempty mempty mempty
  (MkSplit x y z) `mappend` (MkSplit a b c) =
    MkSplit (x <> a)
            (y <> b)
            (z <> c)

-- | Build a 'BSP' out of a 'LineSegment' soup. Currently the first line segment
-- will be taken as the root split.
buildBSP :: (Epsilon a,Floating a,Fractional a,Num a,Ord a,Show a)
         => [LineSegment (Point V2 a)] -> BSP a
buildBSP [] = Empty
buildBSP (split:lines) =
  let splitLine l =
        let startSide = lineSide split (start l)
            endSide = lineSide split (end l)
        in case lineLineIntersection l split of
             Nothing ->
               case startSide of
                 LeftSide -> left l
                 RightSide -> right l
                 _ ->
                   case endSide of
                     LeftSide -> left l
                     RightSide -> right l
                     _ -> coincident l
             Just p ->
               case startSide of
                 LeftSide ->
                   left (LineSegment (start l)
                                     p) <>
                   right (LineSegment p
                                      (end l))
                 RightSide ->
                   right (LineSegment (start l)
                                      p) <>
                   left (LineSegment p
                                     (end l))
                 OnLine ->
                   case lineSide split (end l) of
                     LeftSide -> left l
                     RightSide -> right l
                     _ -> coincident l
      approximatesPoint (LineSegment a b) =
        nearZero (norm (a - b))
      splits =
        filterSplit (not . approximatesPoint)
                    (foldMap splitLine lines)
  in Split (split :| coincidents splits)
           (buildBSP (lefts splits))
           (buildBSP (rights splits))

-- | Determine if a 'Circle' intersects a 'BSP' tree.
circleIntersects :: (Epsilon a, Floating a, Ord a) => BSP a -> Circle a -> Bool
circleIntersects Empty _ = False
circleIntersects (Split cs@(split :| _) l r) c
  | any (circleLineSegmentIntersection c) cs = True -- TODO Optimize with a ray-circle intersection
  | otherwise =
    case lineSide split (circleCenter c) of
      LeftSide -> circleIntersects l c
      RightSide -> circleIntersects r c
      OnLine -> True
