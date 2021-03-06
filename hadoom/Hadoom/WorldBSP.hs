{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Hadoom.WorldBSP (compileBSP) where

import BasePrelude
import Data.TList
import Hadoom.BSP
import Hadoom.Geometry
import Hadoom.World
import Linear
import Linear.Affine

data V :: SceneElementType -> * where
  VVertex :: Point V2 Float -> V TVertex
  VLines :: [LineSegment (Point V2 Float)] -> V a

compileBSP :: PWorld TWorld -> BSP Float
compileBSP = buildBSP . wallsOf

wallsOf :: PWorld TWorld -> [LineSegment (Point V2 Float)]
wallsOf (PWorld w) =
  case collectWalls w of
    VLines ls -> ls
  where collectWalls :: WorldExpr V t -> V t
        collectWalls (Let bindings in_) =
          collectWalls (in_ (fix (tmap collectWalls . bindings)))
        collectWalls (Var v) = v
        collectWalls (Vertex v) = VVertex v
        collectWalls (Wall v1 v2 _ _) =
          case collectWalls v1 of
            VVertex a ->
              case collectWalls v2 of
                VVertex b ->
                  VLines [LineSegment a b]
        collectWalls (World _ walls) =
          traceShow (length walls) $
          VLines (concatMap (\w ->
                               case collectWalls w of
                                 VLines ls -> ls)
                            walls)
        collectWalls _ = VLines []
