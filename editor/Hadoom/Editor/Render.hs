{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor.Render where

import BasePrelude
import Data.List.NonEmpty (NonEmpty(..))
import Hadoom.Editor.SectorBuilder
import Linear
import Linear.Affine
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Vector as D

data EditorState =
  EditorState {esMousePosition :: Point V2 Double
              ,esSectorBuilder :: SectorBuilder
              ,esHoverSector :: Maybe (NonEmpty (Point V2 Double))
              ,esSelectedSector :: Maybe (NonEmpty (Point V2 Double))
              ,esHalfExtents :: V2 Double}

renderVertex :: D.Diagram Cairo.Cairo D.R2
renderVertex = D.lw D.none (D.square (1 / 5))

renderEditor :: EditorState -> D.Diagram Cairo.Cairo D.R2
renderEditor EditorState{..} =
  let P (V2 mouseX mouseY) = esMousePosition
  in mconcat [D.fc D.white
                   (D.translate (pointToR2 esMousePosition)
                                renderVertex)
             ,foldMap (D.lc D.white .
                       D.lwO 2 .
                       renderSector)
                      (sbComplete esSectorBuilder)
             ,foldMap (D.fc D.red . renderSector) esHoverSector
             ,foldMap (D.fc D.white . renderSector) esSelectedSector
             ,foldMap (\(initialPoint :| vertices) ->
                         D.lwO 2
                               (D.lc D.orange
                                     (trailWithEdgeDirections
                                        (D.mapLoc D.wrapLine
                                                  (D.fromVertices
                                                     (map pointToP2
                                                          (initialPoint :
                                                           reverse vertices))))) <>
                                D.lc D.red
                                     (trailWithEdgeDirections
                                        (D.mapLoc D.wrapLine
                                                  (D.fromVertices
                                                     (map pointToP2
                                                          (reverse (take 2
                                                                         (reverse (initialPoint :
                                                                                   reverse (P (V2 mouseX
                                                                                                  mouseY) :
                                                                                            vertices)))))))))))
                      (sbInProgress esSectorBuilder)
             ,gridLines esHalfExtents]

renderSector :: NonEmpty (Point V2 Double) -> D.Diagram Cairo.Cairo D.R2
renderSector vertices =
  let sectorPolygon :: D.Located (D.Trail D.R2)
      sectorPolygon =
        D.mapLoc (D.wrapTrail . D.closeLine)
                 (D.fromVertices
                    (foldMap (\(P (V2 x y)) ->
                                [D.p2 (x,y)])
                             vertices))
  in D.fc D.lightskyblue
          (D.decorateLocatedTrail sectorPolygon
                                  (repeat renderVertex)) <>
     trailWithEdgeDirections sectorPolygon

trailWithEdgeDirections :: D.Located (D.Trail D.R2) -> D.Diagram D.Cairo D.R2
trailWithEdgeDirections sectorPolygon =
  D.strokeLocTrail sectorPolygon <>
  foldMap (\wall ->
             case D.explodeTrail (wall :: D.Located (D.Trail D.R2)) :: [[D.P2]] of
               [p1,p2]:_ ->
                 let v =
                       D.normalized (p2 D..-. p1)
                 in D.moveTo (wall `D.atParam` 0.5)
                             (D.strokeLine
                                (D.lineFromVertices
                                   [D.origin,D.origin D..+^ D.perp v D.^* 0.2]))
               _ -> mempty)
          (D.explodeTrail sectorPolygon)

gridLines :: V2 Double -> D.Diagram D.Cairo D.R2
gridLines (V2 gridHalfWidth gridHalfHeight) =
  D.lc D.white
       (gridLinesIn (negate gridHalfWidth)
                    gridHalfWidth <>
        D.rotate (90 D.@@ D.deg)
                 (gridLinesIn (negate gridHalfHeight)
                              gridHalfHeight))
  where gridLine =
          D.centerX (D.lwO 1
                           (D.strokeLine
                              (D.lineFromVertices [D.p2 (0,0),D.p2 (1,0)])))
        gridLinesIn x y =
          D.dashingO
            [1,1]
            0
            (foldMap (\n ->
                        D.opacity (0.5 +
                                   0.5 *
                                   (fromIntegral (round n `mod` 2 :: Int)))
                                  (D.translate (D.r2 (0,n))
                                               (D.scale len gridLine)))
                     [x .. y])
          where len = y - x

pointToP2 :: Point V2 Double -> D.P2
pointToP2 (P (V2 x y)) = D.p2 (x,y)

pointToR2 :: Point V2 Double -> D.R2
pointToR2 (P (V2 x y)) = D.r2 (x,y)
