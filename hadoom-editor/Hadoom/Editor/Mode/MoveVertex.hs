{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Hadoom.Editor.Mode.MoveVertex (moveVertexMode) where

import BasePrelude hiding (union)
import Hadoom.Editor.GUI
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Util
import Linear
import Linear.Affine
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GTK
import qualified Data.IntMap.Strict as IntMap
import qualified Graphics.UI.Gtk as GTK
import {-# SOURCE #-} Hadoom.Editor.Mode.Default

moveVertexMode :: Frameworks t
               => HadoomGUI
               -> SectorBuilder
               -> IntMap.Key
               -> Point V2 Double
               -> Moment t (Behavior t Diagram)
moveVertexMode gui@HadoomGUI{..} initialSectorBuilder vertexId dragOrigin =
  mdo let switch = once switchToDefault
          active = stepper True (False <$ switch)
      dragComplete <- filterE ((== GTK.RightButton) . mcButton) .
                      whenE active <$>
                      registerMouseReleased guiMap
      mouseMoved <- whenE active <$> registerMotionNotify guiMap
      let widgetSize =
            pure (V2 30 30 ^*
                  50) -- TODO
          originGrid =
            toGridCoords <$>
            (toDiagramCoords <$> widgetSize <*> pure mapExtents <*>
             pure dragOrigin)
          sectorBuilder =
            stepper initialSectorBuilder
                    (moveVertex vertexId initialSectorBuilder <$>
                     (flip (.-.) <$>
                      originGrid <@>
                      (toGridCoords <$>
                       (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                           mouseMoved))))
      switchToDefault <- execute ((\sb ->
                                     FrameworksMoment
                                       (trimB =<<
                                        defaultMode gui sb)) <$>
                                  (sectorBuilder <@ dragComplete))
      let diagram =
            renderSectorsWithSelection <$> sectorBuilder <*>
            pure Nothing
      return (switchB diagram switch)

moveVertex :: IntMap.Key -> SectorBuilder -> V2 Double -> SectorBuilder
moveVertex vertexId sectorBuilder offset =
  sectorBuilder {sbVertices =
                   IntMap.mapWithKey
                     (\vId coords ->
                        if vId == vertexId
                           then coords .+^ offset
                           else coords)
                     (sbVertices sectorBuilder)}
