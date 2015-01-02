{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import BasePrelude
import Data.List.NonEmpty (NonEmpty(..))
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Linear
import Linear.Affine
import Reactive.Banana ((<@), (<@>))
import qualified Diagrams.Backend.Cairo as D
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Render

outputSize :: Num a => V2 a
outputSize =
  V2 gridHalfWidth gridHalfHeight ^*
  (2 * 25)

main :: IO ()
main =
  do _ <- GTK.initGUI
     builder <- GTK.builderNew
     GTK.builderAddFromFile builder "editor/editor.glade"
     w <- GTK.builderGetObject builder GTK.castToWindow "appWindow"
     GTK.widgetShow w
     da <- GTK.builderGetObject builder GTK.castToDrawingArea "mapDrawingArea"
     GTK.widgetAddEvents
       da
       [GTK.PointerMotionMask
       ,GTK.PointerMotionHintMask
       ,GTK.ButtonMotionMask
       ,GTK.ButtonPressMask]
     GTK.widgetSetSizeRequest da
                              (outputSize ^. _x)
                              (outputSize ^. _y)
     gui <- HadoomGUI w <$>
            newIORef mempty <*>
            pure da
     RB.compile (hadoomEditorNetwork gui) >>=
       RB.actuate
     _ <- GTK.on da
                 GTK.draw
                 (do d <- liftIO (readIORef (outRef gui))
                     snd (D.renderDia
                            Cairo.Cairo
                            (Cairo.CairoOptions
                               ""
                               (D.Dims (outputSize ^. _x)
                                       (outputSize ^. _y))
                               Cairo.RenderOnly
                               False)
                            (D.bg D.black d)))
     GTK.mainGUI

data HadoomGUI =
  HadoomGUI {appWindow :: GTK.Window
            ,outRef :: IORef (D.Diagram D.Cairo D.R2)
            ,guiMap :: GTK.DrawingArea}

registerDestroy :: (RB.Frameworks t,GTK.WidgetClass w)
                => w -> RB.Moment t (RB.Event t ())
registerDestroy widget =
  RB.fromAddHandler
    (RB.AddHandler
       (\h ->
          fmap GTK.signalDisconnect
               (GTK.on widget
                       GTK.destroyEvent
                       (False <$
                        liftIO (h ())))))

registerMotionNotify :: (RB.Frameworks t,GTK.WidgetClass w)
                     => w -> RB.Moment t (RB.Event t (Point V2 Double))
registerMotionNotify widget =
  RB.fromAddHandler
    (withEvent (GTK.on widget GTK.motionNotifyEvent)
               (\h ->
                  do (x,y) <- GTK.eventCoordinates
                     False <$
                       liftIO (h (P (V2 x y)))))

registerMouseClicked :: (RB.Frameworks t,GTK.WidgetClass w)
                     => w -> RB.Moment t (RB.Event t GTK.MouseButton)
registerMouseClicked widget =
  RB.fromAddHandler
    (RB.AddHandler
       (\h ->
          fmap GTK.signalDisconnect
               (GTK.on widget
                       GTK.buttonPressEvent
                       (do button <- GTK.eventButton
                           False <$
                             liftIO (h button)))))

hadoomEditorNetwork :: RB.Frameworks t
                    => HadoomGUI -> RB.Moment t ()
hadoomEditorNetwork HadoomGUI{..} =
  do mainWindowClosed <- registerDestroy appWindow
     mouseMoved <- registerMotionNotify guiMap
     mouseClicked <- registerMouseClicked guiMap
     let lmbClicked =
           RB.filterE (== GTK.LeftButton) mouseClicked
         rmbClicked =
           RB.filterE (== GTK.RightButton) mouseClicked
         zoomFactor = pure 10
         widgetSize =
           pure (outputSize :: V2 Double)
         gridCoords =
           RB.stepper 0
                      (RB.filterJust (toGridCoords <$> widgetSize <@> mouseMoved))
         sectorBuilder =
           mkSectorBuilder (gridCoords <@ lmbClicked)
         overSector =
           querySelected <$>
           (sbComplete <$> sectorBuilder) <*>
           (toDiagramCoords <$> widgetSize <*> zoomFactor <*>
            RB.stepper 0 mouseMoved)
         selectedSector =
           RB.stepper Nothing
                      (overSector <@ rmbClicked)
         editorState =
           EditorState <$> gridCoords <*> sectorBuilder <*> overSector <*>
           selectedSector <*>
           pure (V2 gridHalfWidth gridHalfHeight)
         diagram = renderEditor <$> editorState
         shouldRedraw =
           foldl1 RB.union [void mouseMoved,void mouseClicked]
     diagramChanged <- RB.changes diagram
     RB.reactimate'
       (fmap (writeIORef outRef) <$>
        diagramChanged)
     RB.reactimate (GTK.widgetQueueDraw guiMap <$ shouldRedraw)
     RB.reactimate (GTK.mainQuit <$ mainWindowClosed)

querySelected :: [NonEmpty (Point V2 Double)]
              -> Point V2 Double
              -> Maybe (NonEmpty (Point V2 Double))
querySelected sectors (P (V2 x y)) =
  let sectorPaths =
        foldMap (\s ->
                   D.value (First (Just s))
                           (renderSector s))
                sectors
  in getFirst (D.runQuery (D.query sectorPaths)
                          (D.p2 (x,y)))

toDiagramCoords :: V2 Double -> Double -> Point V2 Double -> Point V2 Double
toDiagramCoords (V2 w h) zoom (P (V2 x y)) =
  let (_,t,_) =
        D.adjustDia
          Cairo.Cairo
          (Cairo.CairoOptions ""
                              (D.Dims w h)
                              Cairo.RenderOnly
                              False)
          (D.clipped (D.scale zoom (D.square 100))
                     (D.square (2 * gridHalfWidth)) :: D.Diagram Cairo.Cairo D.R2)
  in case D.coords (D.papply (D.inv t)
                             (D.p2 (x,y))) of
       x' D.:& y' -> P (V2 x' y')

toGridCoords :: V2 Double -> Point V2 Double -> Maybe (Point V2 Double)
toGridCoords (V2 w h) (P (V2 x y)) =
  let (_,_,gridPoints) =
        D.adjustDia
          Cairo.Cairo
          (Cairo.CairoOptions ""
                              (D.Dims w h)
                              Cairo.RenderOnly
                              False)
          (gridIntersections gridHalfHeight gridHalfWidth)
      ps =
        D.runQuery (D.query gridPoints)
                   (D.p2 (x,y))
  in case ps of
       (p:_) -> Just p
       _ -> Nothing

gridIntersection :: Double
                 -> Double
                 -> D.QDiagram Cairo.Cairo D.R2 [Point V2 Double]
gridIntersection x y =
  D.value [P (V2 x y)]
          (D.translate (D.r2 (x,y))
                       (D.square 1))

gridIntersections :: Double
                  -> Double
                  -> D.QDiagram Cairo.Cairo D.R2 [Point V2 Double]
gridIntersections halfH halfW =
  foldMap (\x ->
             foldMap (\y -> gridIntersection x y)
                     [negate halfH .. halfH])
          [negate halfW .. halfW]

withEvent :: (GTK.GObjectClass obj,RB.MonadIO m)
          => (t -> IO (GTK.ConnectId obj))
          -> ((a -> m ()) -> t)
          -> RB.AddHandler a
withEvent f m =
  RB.AddHandler
    (\h ->
       fmap GTK.signalDisconnect (f (m (liftIO . h))))

gridHalfWidth :: Num a => a
gridHalfWidth = 40

gridHalfHeight :: Num a => a
gridHalfHeight = 40
