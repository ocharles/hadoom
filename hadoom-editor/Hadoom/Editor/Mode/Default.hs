{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor.Mode.Default (defaultMode) where

import BasePrelude hiding (union)
import Hadoom.Editor.GUI
import Hadoom.Editor.Mode.CreateSector
import Hadoom.Editor.Mode.MoveSector
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
            once (switchToCreateSector `union` switchToMoveSector)
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
          overSector =
            findSectorKey sectorBuilder <$>
            (stepper 0
                     (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                         mouseMoved))
          completeSectors = renderSectorsWithSelection sectorBuilder <$>
                            overSector
          diagram =
            mappend <$>
            (renderMousePosition <$> gridCoords) <*>
            completeSectors
      switchToCreateSector <- execute ((\m ->
                                          FrameworksMoment
                                            (trimB =<<
                                             (createSectorMode gui sectorBuilder m))) <$>
                                       (gridCoords <@ lmbClicked))
      switchToMoveSector <- execute ((\(sectorId,origin) ->
                                        FrameworksMoment
                                          (trimB =<<
                                           moveSectorMode gui sectorBuilder sectorId origin)) <$>
                                     filterJust
                                       (((\s coords ->
                                            fmap (\s' ->
                                                    (s',coords))
                                                 s) <$>
                                         overSector <@>
                                         (mcCoordinates <$> rmbClicked))))
      return (switchB diagram switch)

findSectorKey :: SectorBuilder -> Point V2 Double -> Maybe IntMap.Key
findSectorKey sectorBuilder (P (V2 x y)) =
  let sectorPaths :: D.QDiagram Cairo.Cairo D.R2 (First IntMap.Key)
      sectorPaths =
        IntMap.foldMapWithKey
          (\sectorId sector ->
             D.value (First (Just sectorId))
                     (D.strokeLocTrail
                        (D.offsetTrail
                           0.5
                           (D.mapLoc (D.wrapTrail . D.closeLine)
                                     (sectorToTrailLike
                                        (fmap (sbVertices sectorBuilder IntMap.!) sector))))))
          (sbSectors sectorBuilder)
  in getFirst (D.runQuery (D.query (sectorPaths))
                          (D.p2 (x,y)))
