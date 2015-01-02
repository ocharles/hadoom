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

data EditorState =
  EditorState {esMousePosition :: Point V2 Double
              ,esSectorBuilder :: SectorBuilder
              ,esHoverSector :: Maybe (NonEmpty (Point V2 Double))
              ,esSelectedSector :: Maybe (NonEmpty (Point V2 Double))
              ,esHalfExtents :: V2 Double}

renderEditor :: EditorState -> D.Diagram Cairo.Cairo D.R2
renderEditor EditorState{..} =
  let P (V2 mouseX mouseY) = esMousePosition
  in mconcat [D.lw D.none
                   (D.fc D.red
                         (D.translate (D.r2 (mouseX,mouseY))
                                      (D.square (1 / 5))))
             ,foldMap (D.lc D.white .
                       D.lwO 2 .
                       renderSector)
                      (sbComplete esSectorBuilder)
             ,foldMap (D.fc D.red . renderSector) esHoverSector
             ,foldMap (D.fc D.white . renderSector) esSelectedSector
             ,case sbInProgress esSectorBuilder of
                Just (initialPoint :| vertices) ->
                  D.lc D.green
                       (D.lwO 2
                              (D.strokeLocLine
                                 (D.fromVertices
                                    (map pointToP2
                                         (initialPoint :
                                          reverse (P (V2 mouseX mouseY) :
                                                   vertices))))))
                Nothing -> mempty
             ,gridLines esHalfExtents]

renderSector :: NonEmpty (Point V2 Double) -> D.Diagram Cairo.Cairo D.R2
renderSector =
  D.strokeLocLoop .
  D.mapLoc D.closeLine .
  D.fromVertices .
  foldMap (\(P (V2 x y)) -> [D.p2 (x,y)])

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
          D.dashingG
            [1 / 20,1 / 20]
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
