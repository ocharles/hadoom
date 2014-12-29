{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom.Geometry where

import Control.Applicative
import Control.Category
import Control.Lens hiding (indices)
import Data.Foldable (any, foldMap)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Linear as L hiding (outer)
import Prelude hiding (any, floor, ceiling, (.), id)
import qualified Data.Vector as V

rayLineIntersection :: (Epsilon a,Fractional a,Ord a)
                    => V2 a -> V2 a -> V2 a -> V2 a -> Maybe (V2 a)
rayLineIntersection p r q q' =
  let s = q' - q
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
           in if 0 <= u && u <= 1
                 then Just (p + r ^* t)
                 else Nothing

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
                 ((rayLineIntersection m
                                       (V2 1 0)
                                       start_
                                       end_)
                 ,s
                 ,e))
              edges
      (Just i,start,end) =
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
                    [start,end]
      containing =
        V.filter (\((_,a),(j,b),(_,c)) ->
                    j /= pIndex &&
                    triangleArea a b c <
                    0 &&
                    pointInTriangle m i p b)
                 ((V.zip3 indexedOuter
                          (V.drop 1 (indexedOuter <> indexedOuter))
                          (V.drop 2 (indexedOuter <> indexedOuter))))
      angleAgainst x =
        dot (V2 1 0) .
        subtract x
      (_,(minimalReflex,_),_) =
        V.minimumBy
          (\(_,(_,a),_) (_,(_,b),_) ->
             foldMap (\f -> f a b)
                     (comparing (angleAgainst m) :
                      comparing (qd m) :
                      []))
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
               [n0,n2,n1] <>
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
        addIndices vertices = V.imap (,) vertices
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

pointInTriangle :: (Epsilon a, Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
pointInTriangle p0@(V2 p0x p0y) p1@(V2 p1x p1y) p2@(V2 p2x p2y) (V2 px py) =
  let area = triangleArea p0 p1 p2
      s = 1 / (2 * area) * (p0y * p2x - p0x * p2y + (p2y - p0y) * px + (p0x - p2x) * py)
      t = 1 / (2 * area) * (p0x * p1y - p0y * p1x + (p0y - p1y) * px + (p1x - p0x) * py)
  in (s > 0 || nearZero s) && (t > 0 || nearZero t) && ((1 - s - t) > 0 || nearZero (1 - s - t))
