{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor.Mode.Default (defaultMode) where

import Hadoom.Editor.Mode.CreateSector
import BasePrelude
import Hadoom.Editor.GUI
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Util
import Linear
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.GTK
import qualified Graphics.UI.Gtk as GTK

defaultMode :: Frameworks t => HadoomGUI -> SectorBuilder -> Moment t (Behavior t Diagram)
defaultMode gui@HadoomGUI{..} sectorBuilder =
  do mouseMoved <- registerMotionNotify guiMap
     mouseClicked <- registerMouseClicked guiMap
     let widgetSize =
           pure (V2 30 30 ^*
                 50) -- TODO
         lmbClicked =
           filterE ((== GTK.LeftButton) . mcButton) mouseClicked
         gridCoords =
           stepper 0
                   (filterJust
                      (toGridCoords mapExtents <$>
                       (toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                           mouseMoved)))
         diagram =
           mappend <$>
           (renderMousePosition <$> gridCoords) <*>
           pure (renderCompleteSectors sectorBuilder)
     switchToCreateSector <- execute ((\m ->
                                         FrameworksMoment
                                           ((=<<) trimB (createSectorMode gui sectorBuilder m))) <$>
                                      (gridCoords <@ lmbClicked))
     return (switchB diagram (once switchToCreateSector))
