{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Hadoom.Editor.SectorBuilder
       (SectorBuilder(..), emptySectorBuilder, equalPoints, insertMax,
        joinPolygon, linesOfPolygon)
       where

import BasePrelude hiding (lines, union)
import Data.IntMap (IntMap)
import Hadoom.Geometry
import Linear
import Linear.Affine
import qualified Data.IntMap.Strict as IntMap

type VertexId = IntMap.Key

data SectorBuilder =
  SectorBuilder {sbVertices :: IntMap (Point V2 Double)
                ,sbSectors :: IntMap (Polygon VertexId)}
  deriving (Show)

emptySectorBuilder :: SectorBuilder
emptySectorBuilder =
  SectorBuilder mempty
                mempty

equalPoints :: (Affine p,Epsilon (Diff p a),Num a)
            => p a -> p a -> Bool
equalPoints a b = nearZero (a .-. b)

insertMax :: IntMap a -> a -> (IntMap a, IntMap.Key)
insertMax im a
  | IntMap.null im = (IntMap.singleton 0 a,0)
  | otherwise =
    let k = succ (fst (IntMap.findMax im))
    in (IntMap.insert k a im,k)


-- TODO Rewrite without relying on partiality (pattern match)
joinPolygon :: Foldable f => Polygon (f a) -> Polygon a
joinPolygon (Polygon a b c ds) =
  let v1:v2:v3:vs = toList a <> toList b <> toList c <> concatMap toList ds
  in Polygon v1 v2 v3 vs

linesOfPolygon :: Polygon a -> Polygon (LineSegment a)
linesOfPolygon (Polygon v1 v2 v3 []) =
  Polygon (LineSegment v1 v2)
          (LineSegment v2 v3)
          (LineSegment v3 v1)
          []
linesOfPolygon (Polygon v1 v2 v3 vs@(v4:vs')) =
  Polygon (LineSegment v1 v2)
          (LineSegment v2 v3)
          (LineSegment v3 v4)
          (zipWith LineSegment
                   vs
                   (vs' <>
                    [v1]))
