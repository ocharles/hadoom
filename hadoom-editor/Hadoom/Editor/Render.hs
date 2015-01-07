{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Hadoom.Editor.Render
       (Diagram, renderCompleteSectors, renderMousePosition, renderGrid,
        renderSector, trailWithEdgeDirections, pointToP2)
       where

import BasePrelude
import Hadoom.Editor.SectorBuilder
import Hadoom.Geometry
import Linear
import Linear.Affine
import qualified Data.IntMap.Strict as IntMap
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D

type Diagram = D.Diagram Cairo.Cairo D.R2

renderVertex :: Diagram
renderVertex = D.lw D.none (D.square (1 / 5))

renderCompleteSectors :: SectorBuilder -> Diagram
renderCompleteSectors sectorBuilder =
  foldMap renderSector
          (fmap (fmap (sbVertices sectorBuilder IntMap.!))
                (sbSectors sectorBuilder))

renderMousePosition :: Point V2 Double -> Diagram
renderMousePosition mousePosition =
  D.fc D.white
       (D.translate (pointToR2 mousePosition)
                    renderVertex)

renderGrid :: V2 Double -> Diagram
renderGrid halfExtents =
  D.lwO 1
        (D.dashingO [1,1]
                    0
                    (renderOrigin <> gridLines halfExtents))

-- | Render a cross hair at the origin. This is used to indicate the origin
-- point of map space.
renderOrigin :: Diagram
renderOrigin =
  D.lc D.yellow
       (mconcat (take 4
                      (iterate (D.rotateBy (1 / 4))
                               (D.strokeLocLine
                                  (D.fromVertices [D.origin,D.p2 (1 / 3,0)])))))

sectorToTrailLike :: (D.TrailLike t,D.V t ~ D.R2)
                  => Polygon (Point V2 Double) -> t
sectorToTrailLike =
  D.fromVertices .
  foldMap (\(P (V2 x y)) -> [D.p2 (x,y)])

renderSector :: Polygon (Point V2 Double) -> Diagram
renderSector vertices =
  let sectorPolygon :: D.Located (D.Trail D.R2)
      sectorPolygon =
        D.mapLoc (D.wrapTrail . D.closeLine) (sectorToTrailLike vertices)

  in D.fc D.lightskyblue
          (D.decorateLocatedTrail sectorPolygon
                                  (repeat renderVertex)) <>
     trailWithEdgeDirections sectorPolygon

trailWithEdgeDirections :: D.Located (D.Trail D.R2) -> Diagram
trailWithEdgeDirections sectorPolygon =
  D.strokeLocTrail sectorPolygon <>
  foldMap (\wall ->
             D.moveTo ((wall :: D.Located (D.Trail D.R2)) `D.atParam`
                       0.5)
                      (D.strokeLine
                         (D.lineFromVertices
                            [D.origin
                            ,D.origin D..+^ (negate (D.normalAtParam wall 0.5 D.^* 0.2))])))
          (D.explodeTrail sectorPolygon)

gridLines :: V2 Double -> Diagram
gridLines (V2 gridHalfWidth gridHalfHeight) =
  D.lc D.white
       (gridLinesIn (negate gridHalfWidth)
                    gridHalfWidth <>
        D.rotate (90 D.@@ D.deg)
                 (gridLinesIn (negate gridHalfHeight)
                              gridHalfHeight))
  where gridLine =
          D.centerX (D.strokeLine (D.lineFromVertices [D.p2 (0,0),D.p2 (1,0)]))
        gridLinesIn x y =
          foldMap (\n ->
                     D.opacity (0.5 +
                                0.5 *
                                (fromIntegral (round n `mod` 2 :: Int)))
                               (D.translate (D.r2 (0,n))
                                            (D.scale len gridLine)))
                  [x .. y]
          where len = y - x

pointToP2 :: Point V2 Double -> D.P2
pointToP2 (P (V2 x y)) = D.p2 (x,y)

pointToR2 :: Point V2 Double -> D.R2
pointToR2 (P (V2 x y)) = D.r2 (x,y)
