module Main where

import Control.Applicative
import Control.Lens ((<&>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.Foldable (foldMap)
import Data.Monoid

import Reactive.Banana ((<@), (<@>))

import Graphics.UI.Gtk as GTK

import qualified Diagrams.Prelude as Diagrams
import qualified Diagrams.Backend.Cairo as Diagrams
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Backend.Gtk as Diagrams
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

main :: IO ()
main =
  do _ <- GTK.initGUI
     w <- GTK.windowNew
     GTK.windowSetTitle w "Hadoom Level Editor"
     GTK.widgetShow w
     dA <- GTK.drawingAreaNew
     GTK.widgetSetSizeRequest dA 100 100
     GTK.containerAdd w dA
     GTK.widgetShow dA
     GTK.widgetAddEvents
       dA
       [GTK.PointerMotionMask,GTK.PointerMotionHintMask,GTK.ButtonMotionMask]
     RB.compile (hadoomEditorNetwork w dA) >>=
       RB.actuate
     GTK.mainGUI

hadoomEditorNetwork :: RB.Frameworks t
                    => GTK.Window -> GTK.DrawingArea -> RB.Moment t ()
hadoomEditorNetwork w dA =
  do shouldRedraw <- RB.fromAddHandler $
                     withEvent (GTK.on dA GTK.exposeEvent) $
                     \h ->
                       False <$
                       h ()
     mainWindowClosed <- RB.fromAddHandler $
                         simpleEvent (GTK.onDestroy w)
     drawableAreaResized <- RB.fromAddHandler $ RB.AddHandler $
                            \h ->
                              fmap signalDisconnect $
                              GTK.on dA GTK.sizeAllocate $
                              \(GTK.Rectangle _ _ wi hi) ->
                                h (wi,hi)
     mouseMoved <- RB.fromAddHandler $
                   withEvent (GTK.on dA GTK.motionNotifyEvent) $
                   \h ->
                     False <$
                     (GTK.eventCoordinates >>= liftIO . h)
     mouseClicked <- RB.fromAddHandler $ RB.AddHandler $
                     \h ->
                       fmap signalDisconnect $
                       GTK.on dA GTK.buttonPressEvent $
                       False <$
                       liftIO (h ())
     let gridCoords =
           RB.stepper (0,0) $
           RB.filterJust $
           (toGridCoords <$>
            RB.stepper (100,100)
                       drawableAreaResized) <@>
           mouseMoved
           where toGridCoords (w,h) (x,y) =
                   let (_,_,gridPoints) =
                         Diagrams.adjustDia
                           Cairo.Cairo
                           (Cairo.CairoOptions
                              ""
                              (Diagrams.Dims (fromIntegral w)
                                             (fromIntegral h))
                              Cairo.RenderOnly
                              False)
                           (gridIntersections 5 5)
                       ps =
                         Diagrams.runQuery (Diagrams.query gridPoints)
                                           (Diagrams.p2 (x,y))
                   in case ps of
                        (p:_) -> Just p
                        _ -> Nothing
         diagram (mouseX,mouseY) (drawnLines,currentLine) =
           mconcat [Diagrams.fc
                       Diagrams.red
                       (Diagrams.translate (Diagrams.r2 (mouseX,mouseY))
                                           (Diagrams.square (1 / 5)))
                   ,foldMap (\(start,end) ->
                               Diagrams.lc Diagrams.white $
                               Diagrams.lwO 2 $
                               Diagrams.strokeLocLine $
                               Diagrams.fromVertices
                                 [Diagrams.p2 start,Diagrams.p2 end])
                            drawnLines
                   ,case currentLine of
                      Just start ->
                        Diagrams.lc Diagrams.green $
                        Diagrams.lwO 2 $
                        Diagrams.strokeLocLine $
                        Diagrams.fromVertices
                          [Diagrams.p2 start,Diagrams.p2 (mouseX,mouseY)]
                      Nothing -> mempty
                   ,gridLines]
         lineSetChanged =
           RB.accumE ([],Nothing)
                     (updateLineSet <$>
                      (gridCoords <@ mouseClicked))
           where updateLineSet coords =
                   \(lines,current) ->
                     case current of
                       Nothing ->
                         (lines,Just coords)
                       Just start ->
                         ((start,coords) :
                          lines
                         ,Just coords)
         lineSets =
           RB.stepper ([],Nothing)
                      lineSetChanged
     RB.reactimate $ GTK.widgetQueueDraw dA <$
       (void mouseMoved `RB.union` void lineSetChanged)
     RB.reactimate $ GTK.mainQuit <$ mainWindowClosed
     RB.reactimate
       (((diagram <$> gridCoords <*> lineSets) <@
         shouldRedraw) <&>
        \d ->
          liftIO $
          (Diagrams.defaultRender dA . Diagrams.bg Diagrams.black .
           Diagrams.clipped
             (Diagrams.scale zoomFactor $
              Diagrams.square 100)) d)

gridIntersection :: Double -> Double -> Diagrams.QDiagram Cairo.Cairo Diagrams.R2 [(Double, Double)]
gridIntersection x y = Diagrams.value [(x, y)] $ Diagrams.translate (Diagrams.r2 (x,y)) (Diagrams.square 1)

gridIntersections :: Double -> Double -> Diagrams.QDiagram Cairo.Cairo Diagrams.R2 [(Double, Double)]
gridIntersections halfH halfW =
  foldMap (\x ->
             foldMap (\y ->
                        gridIntersection x y)
                     [negate halfH .. halfH])
          [negate halfW .. halfW]

simpleEvent f = withEvent f (\h -> h ())

withEvent f m =
  RB.AddHandler $
  \h ->
    fmap signalDisconnect $
    f (m (liftIO . h))

gridLines :: Diagrams.Diagram Diagrams.Cairo Diagrams.R2
gridLines =
  Diagrams.lc
    Diagrams.white
    (gridLinesIn (negate gridHalfWidth)
                 gridHalfWidth <>
     Diagrams.rotate
       (90 Diagrams.@@ Diagrams.deg)
       (gridLinesIn (negate gridHalfHeight)
                    gridHalfHeight))

gridLinesIn :: Double -> Double -> Diagrams.Diagram Diagrams.Cairo Diagrams.R2
gridLinesIn x y = Diagrams.dashingG [1 / 20, 1 / 20] 0 $
  foldMap (\n ->
             Diagrams.opacity
               (0.5 + 0.5 *
                (fromIntegral (round n `mod` 2 :: Int))) $
             Diagrams.translate (Diagrams.r2 (0,n)) $
             Diagrams.scale len $
             gridLine)
          [x .. y]
  where len = y - x

gridHalfWidth, gridHalfHeight, zoomFactor :: Double
(gridHalfWidth, gridHalfHeight) = (1000, 1000)
zoomFactor = 0.1

gridLine :: Diagrams.Diagram Diagrams.Cairo Diagrams.R2
gridLine =
  Diagrams.centerX $
  Diagrams.lwO 1 $
  Diagrams.strokeLine $
  Diagrams.lineFromVertices [Diagrams.p2 (0,0),Diagrams.p2 (1,0)]
