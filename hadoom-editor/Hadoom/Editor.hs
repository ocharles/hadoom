{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor where
import BasePrelude
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Linear
import Linear.Affine
import Reactive.Banana ((<@), (<@>))
import Reactive.Banana.GTK
import qualified Data.IntMap.Strict as IntMap
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Offset as D
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

data HadoomGUI =
  HadoomGUI {appWindow :: GTK.Window
            ,outRef :: IORef (D.Diagram D.Cairo D.R2)
            ,guiMap :: GTK.DrawingArea
            ,mapExtents :: V2 Double
            ,playHadoomButton :: GTK.ToolButton}

-- TODO
outputSize :: Num a => V2 a
outputSize = V2 30 30 ^* 50

editorNetwork :: RB.Frameworks t
              => HadoomGUI -> RB.Moment t ()
editorNetwork HadoomGUI{..} =
  do mainWindowClosed <- registerDestroy appWindow
     mouseMoved <- registerMotionNotify guiMap
     mouseClicked <- registerMouseClicked guiMap
     guiKeyPressed <- registerKeyPressed guiMap
     let lmbClicked =
           RB.filterE (== GTK.LeftButton) mouseClicked
         escapePressed =
           void (RB.filterE (== 65307) guiKeyPressed)
         widgetSize =
           pure (outputSize :: V2 Double)
         diagramCoordsChanged = toDiagramCoords <$> widgetSize <*> pure mapExtents <@>
                                                                   mouseMoved
         gridCoords =
           RB.stepper
             0
             (RB.filterJust (toGridCoords mapExtents <$> diagramCoordsChanged))
         sectorBuilder =
           mkSectorBuilder
             SectorBuilderEvents {evAddVertex = gridCoords <@ lmbClicked
                                 ,evAbort = escapePressed}
         overSector =
           RB.stepper Nothing
                      (findSectorKey <$> sectorBuilder <@> diagramCoordsChanged)
         editorState = EditorState <$> gridCoords <*> sectorBuilder <*>
                       overSector <*> pure mapExtents
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

toGridCoords :: V2 Double -> Point V2 Double -> Maybe (Point V2 Double)
toGridCoords mapExtents (P (V2 x y)) =
  let ps =
        D.runQuery (D.query (gridIntersections mapExtents))
                   (D.p2 (x,y))
  in case ps of
       (p:_) -> Just p
       _ -> Nothing

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
       x' D.:& y' -> P (V2 x' (negate y'))

findSectorKey :: SectorBuilder -> Point V2 Double -> Maybe IntMap.Key
findSectorKey sectorBuilder (P (V2 x y)) =
  let sectorPaths :: D.QDiagram D.Cairo D.R2 (First IntMap.Key)
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
