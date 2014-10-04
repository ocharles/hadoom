{-# LANGUAGE RecordWildCards #-}
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

import qualified Hadoom

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
     dA <- GTK.drawingAreaNew
     GTK.widgetSetSizeRequest dA 800 800
     vbox <- GTK.vBoxNew False 1
     GTK.boxPackStart vbox dA GTK.PackGrow 0
     GTK.widgetShow dA
     play <- GTK.buttonNewWithLabel "Play Hadoom"
     GTK.boxPackStartDefaults vbox play
     GTK.widgetShow play
     GTK.containerAdd w vbox
     GTK.widgetShow vbox
     GTK.widgetAddEvents
       dA
       [GTK.PointerMotionMask,GTK.PointerMotionHintMask,GTK.ButtonMotionMask]
     GTK.widgetShow w
     RB.compile (hadoomEditorNetwork w dA play) >>=
       RB.actuate
     GTK.mainGUI

type Vertex = (Double, Double)

data SectorBuilder = SectorBuilder
  { sbInProgress :: Maybe (Vertex, [Vertex])
  , sbComplete :: [[Vertex]]
  }

emptySectorBuilder :: SectorBuilder
emptySectorBuilder = SectorBuilder Nothing []

updateSectorBuilder :: Vertex -> SectorBuilder -> SectorBuilder
updateSectorBuilder coords sb =
  case sbInProgress sb of
    Nothing ->
      sb {sbInProgress = Just (coords,[])}
    Just (initialPoint,ps)
      | coords /= initialPoint ->
        sb {sbInProgress =
              Just (initialPoint,coords : ps)}
      | otherwise ->
        sb {sbInProgress = Nothing
           ,sbComplete =
              (initialPoint : reverse ps) :
              sbComplete sb}

visualizeMap :: Vertex -> SectorBuilder -> Diagrams.Diagram Cairo.Cairo Diagrams.R2
visualizeMap (mouseX,mouseY) sb =
  mconcat [Diagrams.fc
             Diagrams.red
             (Diagrams.translate (Diagrams.r2 (mouseX,mouseY))
                                 (Diagrams.square (1 / 5)))
          ,foldMap (\vertices ->
                      Diagrams.lc Diagrams.white $
                      Diagrams.lwO 2 $
                      Diagrams.strokeLocLoop $
                      Diagrams.mapLoc Diagrams.closeLine $
                      Diagrams.fromVertices $
                      map Diagrams.p2 vertices)
                   (sbComplete sb)
          ,case sbInProgress sb of
             Just (initialPoint,vertices) ->
               Diagrams.lc Diagrams.green $
               Diagrams.lwO 2 $
               Diagrams.strokeLocLine $
               Diagrams.fromVertices $
               map Diagrams.p2
                   (initialPoint :
                    reverse ((mouseX,mouseY) :
                             vertices))
             Nothing -> mempty
          ,gridLines]

hadoomEditorNetwork :: RB.Frameworks t
                    => GTK.Window -> GTK.DrawingArea -> GTK.Button -> RB.Moment t ()
hadoomEditorNetwork w dA play =
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
     playHadoom <- RB.fromAddHandler $ RB.AddHandler $
                   \h ->
                     fmap signalDisconnect $ GTK.on play GTK.buttonActivated $
                     h ()
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
         sectorBuilderChanged =
           RB.accumE emptySectorBuilder
                     (updateSectorBuilder <$>
                      (gridCoords <@ mouseClicked))
         sectorBuilder =
           RB.stepper emptySectorBuilder sectorBuilderChanged
     RB.reactimate $ Hadoom.testHadoom . head . sbComplete <$> (sectorBuilder <@ playHadoom)
     RB.reactimate $ GTK.widgetQueueDraw dA <$
       (void mouseMoved `RB.union` void sectorBuilderChanged)
     RB.reactimate $ GTK.mainQuit <$ mainWindowClosed
     RB.reactimate
       (((visualizeMap <$> gridCoords <*> sectorBuilder) <@
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
(gridHalfWidth, gridHalfHeight) = (100, 100)
zoomFactor = 0.1

gridLine :: Diagrams.Diagram Diagrams.Cairo Diagrams.R2
gridLine =
  Diagrams.centerX $
  Diagrams.lwO 1 $
  Diagrams.strokeLine $
  Diagrams.lineFromVertices [Diagrams.p2 (0,0),Diagrams.p2 (1,0)]
