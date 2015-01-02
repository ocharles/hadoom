{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor where

import BasePrelude
import Data.List.NonEmpty (NonEmpty(..))
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Linear
import Linear.Affine
import Reactive.Banana ((<@), (<@>))
import Reactive.Banana.GTK
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

data HadoomGUI =
  HadoomGUI {appWindow :: GTK.Window
            ,outRef :: IORef (D.Diagram D.Cairo D.R2)
            ,guiMap :: GTK.DrawingArea
            ,mapExtents :: V2 Double
            }

-- TODO
outputSize :: Num a => V2 a
outputSize = V2 40 40 ^* 25

editorNetwork :: RB.Frameworks t
              => HadoomGUI -> RB.Moment t ()
editorNetwork HadoomGUI{..} =
  do mainWindowClosed <- registerDestroy appWindow
     mouseMoved <- registerMotionNotify guiMap
     mouseClicked <- registerMouseClicked guiMap
     let lmbClicked =
           RB.filterE (== GTK.LeftButton) mouseClicked
         rmbClicked =
           RB.filterE (== GTK.RightButton) mouseClicked
         widgetSize =
           pure (outputSize :: V2 Double)
         gridCoords =
           RB.stepper
             0
             (RB.filterJust
                (toGridCoords <$> widgetSize <*> pure mapExtents <@> mouseMoved))
         sectorBuilder =
           mkSectorBuilder (gridCoords <@ lmbClicked)
         overSector =
           querySelected <$>
           (sbComplete <$> sectorBuilder) <*>
           pure mapExtents <*>
           (toDiagramCoords <$> widgetSize <*> pure mapExtents <*>
            RB.stepper 0 mouseMoved)
         selectedSector =
           RB.stepper Nothing
                      (overSector <@ rmbClicked)
         editorState = EditorState <$> gridCoords <*> sectorBuilder <*>
                       overSector <*> selectedSector <*> pure mapExtents
         diagram = renderEditor <$> editorState
         shouldRedraw =
           foldl1 RB.union [void mouseMoved,void mouseClicked]
     diagramChanged <- RB.changes diagram
     RB.reactimate'
       (fmap (writeIORef outRef) <$>
        diagramChanged)
     RB.reactimate (GTK.widgetQueueDraw guiMap <$ shouldRedraw)
     RB.reactimate (GTK.mainQuit <$ mainWindowClosed)

gridIntersections :: V2 Double -> D.QDiagram Cairo.Cairo D.R2 [Point V2 Double]
gridIntersections (V2 halfW halfH) =
  foldMap (\x ->
             foldMap (\y -> gridIntersection x y)
                     [negate halfH .. halfH])
          [negate halfW .. halfW]
  where gridIntersection x y =
          D.value [P (V2 x y)]
                  (D.translate (D.r2 (x,y))
                               (D.square 1))

toGridCoords :: V2 Double -> V2 Double -> Point V2 Double -> Maybe (Point V2 Double)
toGridCoords (V2 w h) mapExtents (P (V2 x y)) =
  let (_,_,gridPoints) =
        D.adjustDia
          Cairo.Cairo
          (Cairo.CairoOptions ""
                              (D.Dims w h)
                              Cairo.RenderOnly
                              False)
          (gridIntersections mapExtents)
      ps =
        D.runQuery (D.query gridPoints)
                   (D.p2 (x,y))
  in case ps of
       (p:_) -> Just p
       _ -> Nothing

querySelected :: [NonEmpty (Point V2 Double)]
              -> V2 Double
              -> Point V2 Double
              -> Maybe (NonEmpty (Point V2 Double))
querySelected sectors (V2 w h) (P (V2 x y)) =
  let sectorPaths =
        foldMap (\s ->
                   D.value (First (Just s))
                           (renderSector s))
                sectors <>
        D.value mempty (D.rect (2 * w) (2 * h))
  in getFirst (D.runQuery (D.query sectorPaths)
                          (D.p2 (x,y)))

toDiagramCoords :: V2 Double -> V2 Double -> Point V2 Double -> Point V2 Double
toDiagramCoords (V2 w h) (V2 gridHalfWidth gridHalfHeight) (P (V2 x y)) =
  let (_,t,_) =
        D.adjustDia
          Cairo.Cairo
          (Cairo.CairoOptions ""
                              (D.Dims w h)
                              Cairo.RenderOnly
                              False)
          (D.rect (2 * gridHalfWidth)
                  (2 * gridHalfHeight) :: D.Diagram Cairo.Cairo D.R2)
  in case D.coords (D.papply (D.inv t)
                             (D.p2 (x,y))) of
       x' D.:& y' -> P (V2 x' y')
