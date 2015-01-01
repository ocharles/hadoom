{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
module Hadoom.Geometry where

import BasePrelude
import Control.Lens hiding (indices)
import Data.Ord (comparing)
import Linear as L hiding (outer)
import qualified Data.Vector as V

data LineSegment a =
  LineSegment {start :: V2 a
              ,end :: V2 a}
  deriving (Eq,Ord,Read,Show)

-- | Given two line segments, find the point of intersection on the first line
-- that intersects with the second line. If the lines do not cross or are
-- coincident, then 'Nothing' is returned.
lineLineIntersection :: (Epsilon a,Fractional a,Ord a)
                    => LineSegment a -> LineSegment a -> Maybe (V2 a)
lineLineIntersection (LineSegment p p') (LineSegment q q') =
  let r = p' - p
      s = q' - q
      cross2 (V2 a b) (V2 x y) = a * y - b * x
      pToQ = q - p
      tNum = pToQ `cross2` s
      uNum = pToQ `cross2` r
  in case r `cross2` s of
       denom
         | nearZero denom -> Nothing
         | otherwise ->
           let u = uNum / denom
               t = tNum / denom
           in if (0 <= t && t <= 1) && (0 <= u && u <= 1)
                 then Just (p + r ^* t)
                 else Nothing

-- | Determine which side of a line a point lies. If the point is on the line
-- (or extremely close to being so, as defined by 'Epsilon'), then return that
-- information.
lineSide :: (Epsilon a,Ord a)
         => LineSegment a -> V2 a -> LineSide
lineSide (LineSegment (V2 ax ay) (V2 bx by)) (V2 x y) =
  let p = (bx - ax) * (y - ay) - (by - ay) * (x - ax)
  in if | nearZero p -> OnLine
        | p > 0 -> LeftSide
        | p < 0 -> RightSide

data LineSide
  = LeftSide
  | RightSide
  | OnLine
  deriving (Eq,Ord,Read,Show,Enum,Bounded)


makeSimple :: (Epsilon a,Fractional a,Ord a)
           => V.Vector (V2 a) -> V.Vector (V2 a) -> V.Vector (V2 a)
makeSimple inner outer =
  let xMost = comparing (view _x)
      m = V.maximumBy xMost inner
      mIndex = V.maxIndexBy xMost inner
      indexedOuter = V.imap (,) outer
      edges =
        V.zip (V.imap (,) outer)
              (V.tail indexedOuter <> indexedOuter)
      intersections =
        V.map (\(s@(_,start_),e@(_,end_)) ->
                 (lineLineIntersection (LineSegment m (V2 1 0))
                                       (LineSegment start_ end_)
                 ,s
                 ,e))
              edges
      (Just i,lStart,lEnd) =
        V.minimumBy
          (\(x,_,_) (y,_,_) ->
             case (x,y) of
               (Nothing,Nothing) -> EQ
               (Just _,Nothing) -> LT
               (Nothing,Just _) -> GT
               (Just a,Just b) ->
                 comparing (qd m) a b)
          intersections
      (pIndex,p) =
        V.maximumBy (xMost `on` snd)
                    (V.fromList [lStart,lEnd])
      containing =
        V.filter (\((_,a),(j,b),(_,c)) ->
                    j /= pIndex &&
                    triangleArea a b c <
                    0 &&
                    pointInTriangle m i p b)
                 (V.zip3 indexedOuter
                         (V.drop 1 (indexedOuter <> indexedOuter))
                         (V.drop 2 (indexedOuter <> indexedOuter)))
      angleAgainst x =
        dot (V2 1 0) .
        subtract x
      (_,(minimalReflex,_),_) =
        V.minimumBy
          (\(_,(_,a),_) (_,(_,b),_) ->
             foldMap (\f -> f a b)
                     [comparing (angleAgainst m),comparing (qd m)])
          containing
      splitOuter
      -- | nearZero (i - start) = error "makeSimple: startIndex"
      -- | nearZero (i - end) = error "makeSimple: endIndex"
        | V.null containing = pIndex
        | otherwise = minimalReflex
  in case V.splitAt splitOuter outer of
       (before,after) ->
         before <>
         V.take 1 after <>
         V.take (succ (V.length inner))
                (V.drop mIndex inner <>
                 inner) <>
         after

triangulate :: (Epsilon a,Fractional a,Ord a)
            => V.Vector (V2 a) -> V.Vector Int
triangulate = collapseAndTriangulate
  where collapseAndTriangulate vs =
          collapse vs (go (addIndices vs))
        collapse vs =
          V.map (\i ->
                   let v = vs V.! i
                   in fst (V.head (V.filter (nearZero . (v -) . snd)
                                            (V.imap (,) vs))))
        go s
          | V.length s < 3 = empty
          | otherwise =
            do (v0@(n0,_),(n1,_),v2@(n2,_),others) <- takeFirst isEar (separate s)
               V.fromList [n0,n2,n1] <>
                 go (v0 `V.cons`
                     (v2 `V.cons` others))
        takeFirst f =
          V.take 1 .
          V.filter f
        isEar ((_,a),(_,b),(_,c),otherVertices) =
          let area = triangleArea a b c
              containsOther =
                any (pointInTriangle a b c .
                     snd)
                    (V.filter (\(_,v) ->
                                 not (nearZero (a - v)) &&
                                 not (nearZero (b - v)) &&
                                 not (nearZero (c - v)))
                              otherVertices)
          in area > 0 && not containsOther
        addIndices = V.imap (,)
        separate vertices =
          let n = V.length vertices
              doubleVerts = vertices <> vertices
          in V.zip4 vertices
                    (V.drop 1 doubleVerts)
                    (V.drop 2 doubleVerts)
                    (V.imap (\i _ ->
                               V.take (n - 3)
                                      (V.drop (i + 3) doubleVerts))
                            vertices)

triangleArea :: Fractional a => V2 a -> V2 a -> V2 a -> a
triangleArea a b c =
  let toV3 (V2 x y) = V3 x y 1
      det =
        det33 (V3 (toV3 a)
                  (toV3 b)
                  (toV3 c))
  in 0.5 * det

data Circle a =
  Circle {circleCenter :: V2 a
         ,circleRadius :: a}
  deriving (Eq,Ord,Read,Show)

pointInTriangle :: (Epsilon a, Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
pointInTriangle p0@(V2 p0x p0y) p1@(V2 p1x p1y) p2@(V2 p2x p2y) (V2 px py) =
  let area = triangleArea p0 p1 p2
      s = 1 / (2 * area) * (p0y * p2x - p0x * p2y + (p2y - p0y) * px + (p0x - p2x) * py)
      t = 1 / (2 * area) * (p0x * p1y - p0y * p1x + (p0y - p1y) * px + (p1x - p0x) * py)
  in (s > 0 || nearZero s) && (t > 0 || nearZero t) && ((1 - s - t) > 0 || nearZero (1 - s - t))

-- https://stackoverflow.com/questions/1073336/circle-line-segment-collision-detection-algorithm
circleLineSegmentIntersection :: (Floating a, Ord a) => Circle a -> LineSegment a -> Bool
circleLineSegmentIntersection (Circle x r) (LineSegment e l) =
  let d = l - e
      f = e - x
      a = d `dot` d
      b = 2 * (f `dot` d)
      c = (f `dot` f) - (r * r)
      disc = b * b - 4 * a * c
      discRoot = sqrt disc
      t1 = (negate b - discRoot) / (2 * a)
      t2 = (negate b + discRoot) / (2 * a)
  in if | disc < 0 -> False
        | otherwise -> (t1 >= 0 && t1 <= 1) || (t2 >= 0 && t2 <= 1)
