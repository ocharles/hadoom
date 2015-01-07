{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor.Mode.Default (defaultMode) where

import BasePrelude hiding (union)
import Hadoom.Editor.GUI
import Hadoom.Editor.Mode.CreateSector
import Hadoom.Editor.Mode.MoveSector
import Hadoom.Editor.Mode.MoveVertex
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Util
import Linear
import Linear.Affine hiding (origin)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GTK
import qualified Data.IntMap.Strict as IntMap
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Offset as D
import qualified Graphics.UI.Gtk as GTK

defaultMode :: Frameworks t => HadoomGUI -> SectorBuilder -> Moment t (Behavior t Diagram)
defaultMode gui@HadoomGUI{..} sectorBuilder =
  mdo let switch =
            once (switchToCreateSector `union` switchToMove)
          active = stepper True (False <$ switch)
      mouseMoved <- whenE active <$> registerMotionNotify guiMap
      mouseClicked <- whenE active <$> registerMouseClicked guiMap
      let widgetSize =
            pure (V2 30 30 ^*
                  50) -- TODO
          lmbClicked =
            filterE ((== GTK.LeftButton) . mcButton) mouseClicked
          rmbClicked =
            filterE ((== GTK.RightButton) . mcButton) mouseClicked
          gridCoords =
            stepper 0
                    (toGridCoords <$>
                     (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                         mouseMoved))
          hovering =
            findSelectable sectorBuilder <$>
            (stepper 0
                     (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                         mouseMoved))
          completeSectors =
            renderSectorsWithSelection sectorBuilder <$>
            pure Nothing -- TODO
          diagram =
            mappend <$>
            (renderMousePosition <$> gridCoords) <*>
            completeSectors
      switchToCreateSector <- execute ((\m ->
                                          FrameworksMoment
                                            (trimB =<<
                                             (createSectorMode gui sectorBuilder m))) <$>
                                       (gridCoords <@ lmbClicked))
      switchToMove <- execute ((\(selection,origin) ->
                                  FrameworksMoment
                                    (trimB =<<
                                     case selection of
                                       SelectSector sectorId ->
                                         moveSectorMode gui sectorBuilder sectorId origin
                                       SelectVertex vertexId ->
                                         moveVertexMode gui sectorBuilder vertexId origin)) <$>
                               filterJust
                                 (((\s coords ->
                                      fmap (\s' ->
                                              (s',coords))
                                           s) <$>
                                   hovering <@>
                                   (mcCoordinates <$> rmbClicked))))
      return (switchB diagram switch)

data Selectable
  = SelectSector IntMap.Key
  | SelectVertex IntMap.Key
  deriving (Eq,Ord,Read,Show)

findSelectable :: SectorBuilder -> Point V2 Double -> Maybe Selectable
findSelectable sectorBuilder (P (V2 x y)) =
  let sectorPaths =
        IntMap.foldMapWithKey
          (\sectorId sector ->
             D.value (First (Just (SelectSector sectorId)))
                     (D.strokeLocTrail
                        (D.offsetTrail
                           0.5
                           (D.mapLoc (D.wrapTrail . D.closeLine)
                                     (sectorToTrailLike
                                        (fmap (sbVertices sectorBuilder IntMap.!) sector))))))
          (sbSectors sectorBuilder)
      vertices =
        IntMap.foldMapWithKey
          (\vertexId coords ->
             D.value (First (Just (SelectVertex vertexId)))
                     (D.moveTo (pointToP2 coords)
                               (D.circle 0.5)))
          (sbVertices sectorBuilder)
  in getFirst (D.runQuery (D.query (sectorPaths <> vertices :: D.QDiagram Cairo.Cairo D.R2 (First Selectable)))
                          (D.p2 (x,y)))
