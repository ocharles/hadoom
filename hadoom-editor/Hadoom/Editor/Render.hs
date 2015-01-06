{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Hadoom.Editor.Render where

import BasePrelude
import Data.List.NonEmpty (NonEmpty(..))
import Hadoom.Editor.SectorBuilder
import Hadoom.Geometry
import Linear
import Linear.Affine
import qualified Data.IntMap.Strict as IntMap
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D

data EditorState =
  EditorState {esMousePosition :: Point V2 Double
              ,esSectorBuilder :: SectorBuilder
              ,esSelectedSector :: Maybe Int
              ,esHalfExtents :: V2 Double}

type Diagram = D.Diagram Cairo.Cairo D.R2

renderVertex :: Diagram
renderVertex = D.lw D.none (D.square (1 / 5))

-- TODO Rendering the sector builder should not care where the mouse is.
renderSectorBuilder :: Point V2 Double -> SectorBuilder -> Maybe IntMap.Key -> Diagram
renderSectorBuilder mousePosition sb selectedSector =
  case sbState sb of
    AddSector vIds -> renderInProgress vIds <> renderComplete
    _ ->
      D.lwO 1
            (IntMap.foldMapWithKey
               (\sectorId sector ->
                  D.lc (fromMaybe D.white
                                  (D.orange <$
                                   mfilter (== sectorId) selectedSector))
                       (renderSector ((sbVertices sb IntMap.!) <$> sector)))
               (sbSectors sb))
  where renderInProgress vIds =
          let lineWithNormals = trailWithEdgeDirections . D.mapLoc D.wrapLine .
                                                          D.fromVertices .
                                                          map pointToP2
              renderSides (initialPoint :| vertices) =
                D.lwO 1
                      (D.lc D.orange
                            (lineWithNormals (initialPoint : reverse vertices)) <>
                       D.lc D.red
                            (lineWithNormals
                               (reverse (take 2
                                              (reverse (initialPoint :
                                                        reverse (mousePosition :
                                                                 vertices)))))))
              renderLineLengths (initialPoint :| vertices) =
                D.fontSizeO
                  10
                  (D.fc D.orange
                        (foldMap (\wall ->
                                    D.moveTo ((wall :: D.Located (D.Trail D.R2)) `D.atParam`
                                              0.3)
                                             ((case D.explodeTrail wall :: [[D.P2]] of
                                                 [[p1,p2]] ->
                                                   D.text (show (round (D.magnitude
                                                                          (p1 D..-.
                                                                           p2)) :: Int))
                                                 _ -> mempty) <>
                                              D.fc D.black (D.circle (2 / 5))))
                                 (D.explodeTrail
                                    (D.mapLoc D.wrapLine
                                              (D.fromVertices
                                                 (map pointToP2
                                                      (initialPoint :
                                                       reverse (mousePosition :
                                                                vertices))))))))
          in let vertices = (sbVertices sb IntMap.!) <$> vIds
             in renderLineLengths vertices <> renderSides vertices
        renderComplete =
          D.lwO 1
                (foldMap (D.lc D.white . renderSector)
                         (fmap (fmap (sbVertices sb IntMap.!))
                               (sbSectors sb)))

renderEditor :: EditorState -> Diagram
renderEditor EditorState{..} =
  mconcat [renderMousePosition
          ,renderSectorBuilder esMousePosition esSectorBuilder esSelectedSector
          ,renderGrid]
  where renderMousePosition =
          D.fc D.white
               (D.translate (pointToR2 esMousePosition)
                            renderVertex)
        renderGrid =
          D.lwO 1
                (D.dashingO [1,1]
                            0
                            (renderOrigin <> gridLines esHalfExtents))

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
