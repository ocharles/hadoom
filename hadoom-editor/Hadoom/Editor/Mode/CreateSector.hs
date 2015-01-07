{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}
module Hadoom.Editor.Mode.CreateSector (createSectorMode) where

import BasePrelude hiding (union)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sequence (ViewR(..), (|>))
import Hadoom.Editor.GUI
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Util
import Hadoom.Geometry
import Linear
import Linear.Affine
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GTK
import qualified Data.IntMap.Strict as IntMap
import qualified Data.NonEmpty as NE
import qualified Data.Sequence as Seq
import qualified Diagrams.Prelude as D
import qualified Graphics.UI.Gtk as GTK
import {-# SOURCE #-} Hadoom.Editor.Mode.Default

createSectorMode :: Frameworks t
                 => HadoomGUI
                 -> SectorBuilder -- ^ The previous 'SectorBuilder'
                 -> Point V2 Double -- ^ The coordinates of the first vertex
                 -> Moment t (Behavior t Diagram)
createSectorMode gui@HadoomGUI{..} initialSectorBuilder firstVertex =
  mdo let switch = once switchToDefault
          active = stepper True (False <$ switch)
      mouseClicked <- filterE ((== GTK.LeftButton) . mcButton) .
                      whenE active <$>
                      registerMouseClicked guiMap
      mouseMoved <- whenE active <$> registerMotionNotify guiMap
      guiKeyPressed <- whenE active <$> registerKeyPressed guiMap
      let sectorBuilderChanged =
            accumE initialState
                   (addVertex <$>
                    (toGridCoords <$>
                     (toDiagramCoords <$> widgetSize <*>
                      pure mapExtents <@>
                      (mcCoordinates <$> mouseClicked))))
          widgetSize =
            pure (V2 30 30 ^*
                  50) -- TODO
          (completeAbort,completeSuccess) =
            split (filterJust (complete <$> sectorBuilderChanged))
          escapePressed =
            void (filterE (== 65307) guiKeyPressed)
          abort = completeAbort `union` void escapePressed
          gridCoords =
            stepper 0
                    (toGridCoords <$>
                     (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                         mouseMoved))
          newSectorDiagram =
            ((\(sb,vertices) gc ->
                renderInProgress
                  (case (sbVertices sb IntMap.!) <$> vertices of
                     NE.Cons v1 vs ->
                       NE.Cons v1 (vs |> gc))) <$>
             stepper initialState sectorBuilderChanged <*>
             gridCoords)
          completeSectorsDiagram =
            renderCompleteSectors <$>
            stepper initialSectorBuilder (fst <$> sectorBuilderChanged)
          diagram = mappend <$> newSectorDiagram <*> completeSectorsDiagram
      switchToDefault <- execute ((\sb ->
                                     FrameworksMoment
                                       (trimB =<<
                                        defaultMode gui sb)) <$>
                                  completeSuccess `union`
                                  (initialSectorBuilder <$ abort))
      return (switchB diagram switch)
  where initialState =
          let (sbVertices',v1Id) =
                insertMax (sbVertices initialSectorBuilder) firstVertex
          in (splitExistingLines (initialSectorBuilder {sbVertices = sbVertices'})
                                 (v1Id,firstVertex)
             ,pure v1Id)

renderInProgress :: NE.T Seq.Seq (Point V2 Double) -> Diagram
renderInProgress vertices@(NE.viewR -> ((Seq.viewr -> vs :> vm),vn)) =
  let lineWithNormals = trailWithEdgeDirections . D.mapLoc D.wrapLine .
                                                  D.fromVertices . map pointToP2
      renderSides =
        D.lc D.orange (lineWithNormals [vm,vn]) <>
        D.lc D.red (lineWithNormals (toList (vs :> vm)))
      renderLineLengths =
        D.fontSizeO
          10
          (D.fc D.orange
                (foldMap (\wall ->
                            D.moveTo ((wall :: D.Located (D.Trail D.R2)) `D.atParam`
                                      0.3)
                                     ((case D.explodeTrail wall :: [[D.P2]] of
                                         [[p1,p2]] ->
                                           D.text (show (round (D.magnitude
                                                                  (p1 D..-. p2)) :: Int))
                                         _ -> mempty) <>
                                      D.lw D.none (D.fc D.black (D.circle (2 / 5)))))
                         (D.explodeTrail
                            (D.mapLoc D.wrapLine (D.fromVertices (map pointToP2 (toList vertices)))))))
  in renderLineLengths <> renderSides
renderInProgress _ = mempty

-- | Attempt to complete building a sector. If the sector isn't closed, return
-- 'Nothing'. If the sector is closed, but is not 4 or more vertices, return
-- 'Left' '()'. If the sector is complete and valid, return a new 'Right'
-- 'SectorBuilder'.
complete :: (SectorBuilder,NE.T Seq.Seq Int) -> Maybe (Either () SectorBuilder)
complete (sb,NE.Cons v1 vs) =
  case Seq.viewr vs of
    _ :> vn
      | equalPoints (lookupVertex v1)
                    (lookupVertex vn) ->
        Just (if Seq.length vs > 2
                 then Right (sb {sbSectors =
                                   fst (insertMax (sbSectors sb)
                                                  (case Seq.viewr vs of
                                                     vs' :> _ ->
                                                       case toList vs' of
                                                         v2:v3:vs'' ->
                                                           Polygon v1 v2 v3 vs''))})
                 else Left ())
    _ -> Nothing
  where lookupVertex = (sbVertices sb IntMap.!)

addVertex :: Point V2 Double
          -> (SectorBuilder,NE.T Seq.Seq Int)
          -> (SectorBuilder,NE.T Seq.Seq Int)
addVertex coords (sectorBuilder,NE.Cons v1 vs) =
  let (sbVertices',vId) =
        insertMax (sbVertices sectorBuilder) coords
      sb =
        splitExistingLines (sectorBuilder {sbVertices = sbVertices'})
                           (vId,coords)
  in (sb,NE.Cons v1 (vs |> vId))

splitExistingLines :: SectorBuilder
                   -> (IntMap.Key,Point V2 Double)
                   -> SectorBuilder
splitExistingLines sb (pId,p) =
  sb {sbSectors =
        fmap splitPolygon (sbSectors sb)}
  where splitPolygon s =
          let splitLine ls
                | pointOnLine (fmap (sbVertices sb IntMap.!) ls)
                              p =
                  start ls :|
                  [pId]
                | otherwise = start ls :| []
          in joinPolygon (splitLine <$> linesOfPolygon s)
