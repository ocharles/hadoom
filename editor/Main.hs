{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Lens ((<&>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Foldable (foldMap)
import Data.Monoid
import Reactive.Banana ((<@), (<@>))

import qualified Hadoom

import qualified Graphics.UI.Gtk as GTK
import qualified Diagrams.Prelude as Diagrams
import qualified Diagrams.Backend.Cairo as Diagrams
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified DiagramsGtk as Diagrams
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

main :: IO ()
main = do
  _ <- GTK.initGUI

  builder <- GTK.builderNew
  GTK.builderAddFromFile builder "editor/editor.glade"
  w <- GTK.builderGetObject builder GTK.castToWindow "applicationwindow1"
  GTK.widgetShow w

  layout <- GTK.builderGetObject builder GTK.castToLayout "layout1"
  GTK.widgetAddEvents
    layout
    [GTK.PointerMotionMask
    ,GTK.PointerMotionHintMask
    ,GTK.ButtonMotionMask
    ,GTK.ButtonPressMask]

  gui <- HadoomGUI <$> pure layout
                  <*> GTK.builderGetObject builder GTK.castToEventBox "eventbox1"
                  <*> GTK.builderGetObject builder GTK.castToImage "image1"
                  <*> GTK.builderGetObject builder GTK.castToToolButton "toolbutton1"
  RB.compile (hadoomEditorNetwork w gui) >>= RB.actuate
  GTK.mainGUI

data HadoomGUI = HadoomGUI
  { guiMap :: GTK.Layout
  , guiWallTexture :: GTK.EventBox
  , guiWallTextureImage :: GTK.Image
  , guiTest :: GTK.ToolButton
  }

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
      sb {sbInProgress = Just (coords, [])}

    Just (initialPoint,ps)
      | coords /= initialPoint ->
        sb {sbInProgress =
                Just (initialPoint
                     ,coords : ps)}
      | otherwise ->
        sb {sbInProgress =
                Nothing
           ,sbComplete =
                (initialPoint : reverse ps) :
                sbComplete sb}

visualizeMap :: Vertex -> SectorBuilder -> Maybe [Vertex] -> Maybe [Vertex] -> Diagrams.Diagram Cairo.Cairo Diagrams.R2
visualizeMap (mouseX,mouseY) sb overSector selectedSector =
  mconcat [Diagrams.fc
             Diagrams.red
             (Diagrams.translate
                (Diagrams.r2
                   (mouseX
                   ,mouseY))
                (Diagrams.square (1 / 5)))
          ,foldMap (Diagrams.lc Diagrams.white .
                    Diagrams.lwO 2 .
                    sectorDiagram)
                   (sbComplete sb)
          ,foldMap (Diagrams.fc Diagrams.red . sectorDiagram) overSector
          ,foldMap (Diagrams.fc Diagrams.white . sectorDiagram) selectedSector
          ,case sbInProgress sb of
             Just (initialPoint,vertices) ->
               Diagrams.lc Diagrams.green $
               Diagrams.lwO 2 $
               Diagrams.strokeLocLine $
               Diagrams.fromVertices $
               map Diagrams.p2
                   (initialPoint :
                    reverse ((mouseX
                             ,mouseY) :
                             vertices))

             Nothing ->
               mempty
          ,gridLines]

sectorDiagram :: [Vertex] -> Diagrams.Diagram Cairo.Cairo Diagrams.R2
sectorDiagram = Diagrams.strokeLocLoop . Diagrams.mapLoc Diagrams.closeLine .
                Diagrams.fromVertices . map Diagrams.p2

hadoomEditorNetwork :: RB.Frameworks t => GTK.Window -> HadoomGUI -> RB.Moment t ()
hadoomEditorNetwork w HadoomGUI{..} = do
  widgetExposed <-
     RB.fromAddHandler $ RB.AddHandler $ \h ->
       fmap GTK.signalDisconnect $
       GTK.on guiMap GTK.draw $
         liftIO (h ())

  mainWindowClosed <-
    RB.fromAddHandler $ RB.AddHandler $ \h ->
    fmap GTK.signalDisconnect $
    GTK.on w GTK.destroyEvent $
    False <$ liftIO (h ())

  drawableAreaResized <-
    RB.fromAddHandler $ RB.AddHandler $ \h ->
      fmap GTK.signalDisconnect $
      GTK.on guiMap GTK.sizeAllocate $
      \(GTK.Rectangle _ _ wi hi) -> h (wi,hi)

  mouseMoved <-
    RB.fromAddHandler $
      withEvent (GTK.on guiMap GTK.motionNotifyEvent) $ \h ->
        False <$ (GTK.eventCoordinates >>= liftIO . h)

  mouseClicked <-
    RB.fromAddHandler $ RB.AddHandler $ \h ->
      fmap GTK.signalDisconnect $
      GTK.on guiMap GTK.buttonPressEvent $ do
        button <- GTK.eventButton
        False <$ liftIO (h button)

  playHadoom <-
    RB.fromAddHandler $ RB.AddHandler $ \h ->
      fmap GTK.signalDisconnect $
      GTK.onToolButtonClicked guiTest $ h ()

  wallTextureDblClick <-
    RB.fromAddHandler $ RB.AddHandler $ \h ->
      fmap GTK.signalDisconnect $ GTK.on guiWallTexture GTK.buttonPressEvent $ do
        click <- GTK.eventClick
        False <$ when (click == GTK.DoubleClick) (liftIO $ h ())

  let lmbClicked = RB.filterE (== GTK.LeftButton) mouseClicked
      rmbClicked = RB.filterE (== GTK.RightButton) mouseClicked

      zoomFactor = pure 0.1

      widgetSize =
        RB.stepper (100,100) drawableAreaResized

      gridCoords =
        RB.stepper (0,0) . RB.filterJust $
          toGridCoords <$> widgetSize <@> mouseMoved

      sectorBuilderChanged =
        RB.accumE emptySectorBuilder
                  (updateSectorBuilder <$>
                   (gridCoords <@ lmbClicked))

      sectorBuilder =
        RB.stepper emptySectorBuilder sectorBuilderChanged

      overSector =
        querySelected
          <$> (sbComplete <$> sectorBuilder)
          <*> (toDiagramCoords <$> widgetSize <*> zoomFactor <*> RB.stepper (0,0) mouseMoved)

      selectedSector = RB.stepper Nothing (overSector <@ rmbClicked)

      renderChanged =
        (visualizeMap <$> gridCoords <*> sectorBuilder <*> overSector <*> selectedSector) <@ widgetExposed

      shouldRedraw = foldl1 RB.union [ void mouseMoved, void mouseClicked ]

  wallTextureChanged <- fmap RB.filterJust $ RB.execute $ wallTextureDblClick <&> \_ -> RB.FrameworksMoment $ liftIO $ do
    d <- GTK.fileChooserDialogNew (Nothing :: Maybe String)
                                 Nothing GTK.FileChooserActionOpen
                                 [("Cancel", GTK.ResponseCancel), ("Open", GTK.ResponseOk)]
    r <- GTK.dialogRun d
    fp <- GTK.fileChooserGetFilename d
    GTK.widgetDestroy d
    (guard (r == GTK.ResponseOk) *>) <$> return fp

  let wallTexture = RB.stepper "test-texture.jpg" wallTextureChanged

  RB.reactimate $
   wallTextureChanged <&> \fp ->
     GTK.imageSetFromFile guiWallTextureImage fp

  RB.reactimate $
    (Hadoom.testHadoom <$> (head . sbComplete <$> sectorBuilder)
                       <*> wallTexture) <@ playHadoom

  RB.reactimate $
    GTK.widgetQueueDraw guiMap <$ shouldRedraw

  RB.reactimate $
    GTK.mainQuit <$ mainWindowClosed

  RB.reactimate $
    (((,) <$> zoomFactor) <@> renderChanged) <&> \(z,d) -> liftIO $
      Diagrams.defaultRender guiMap $ Diagrams.bg Diagrams.black $
      Diagrams.clipped (Diagrams.scale z $ Diagrams.square 100) $
      d

querySelected :: [[Vertex]] -> (Double, Double) -> Maybe [Vertex]
querySelected sectors xy =
  let sectorPaths =
        foldMap (\s ->
                   Diagrams.value (First (Just s))
                                  (sectorDiagram s))
                sectors
  in getFirst $
     Diagrams.runQuery (Diagrams.query sectorPaths)
                       (Diagrams.p2 xy)

toDiagramCoords :: Integral a => (a, a) -> Double -> (Double, Double) -> (Double, Double)
toDiagramCoords (w,h) zoom xy =
  let (_,t,_) =
        Diagrams.adjustDia
          Cairo.Cairo
          (Cairo.CairoOptions
             ""
             (Diagrams.Dims (fromIntegral w)
                            (fromIntegral h))
             Cairo.RenderOnly
             False)
          (Diagrams.clipped
             (Diagrams.scale zoom $
              Diagrams.square 100)
             (Diagrams.square (2 * gridHalfWidth)) :: Diagrams.Diagram Cairo.Cairo Diagrams.R2)
  in case Diagrams.coords
            (Diagrams.papply (Diagrams.inv t)
                             (Diagrams.p2 xy)) of
       x' Diagrams.:& y' ->
         (x'
         ,y')

toGridCoords :: Integral a => (a, a) -> (Double, Double) -> Maybe (Double, Double)
toGridCoords (w,h) (x,y) =
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
        Diagrams.runQuery
          (Diagrams.query gridPoints)
          (Diagrams.p2
             (x
             ,y))
  in case ps of
       (p:_) ->
         Just p

       _ ->
         Nothing

gridIntersection :: Double -> Double -> Diagrams.QDiagram Cairo.Cairo Diagrams.R2 [(Double, Double)]
gridIntersection x y =
  Diagrams.value [(x ,y)]
    (Diagrams.translate
       (Diagrams.r2 (x ,y))
       (Diagrams.square 1))

gridIntersections :: Double -> Double -> Diagrams.QDiagram Cairo.Cairo Diagrams.R2 [(Double, Double)]
gridIntersections halfH halfW =
  foldMap (\x ->
             foldMap (\y ->
                        gridIntersection x y)
                     [negate halfH .. halfH])
          [negate halfW .. halfW]

simpleEvent :: (GTK.GObjectClass obj,RB.MonadIO m) => (m () -> IO (GTK.ConnectId obj)) -> RB.AddHandler ()
simpleEvent f = withEvent f (\h -> h ())

withEvent :: (GTK.GObjectClass obj,RB.MonadIO m) => (t -> IO (GTK.ConnectId obj)) -> ((a -> m ()) -> t) -> RB.AddHandler a
withEvent f m =
  RB.AddHandler $ \h ->
    fmap GTK.signalDisconnect $
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

gridHalfWidth, gridHalfHeight :: Double
(gridHalfWidth, gridHalfHeight) = (100, 100)

gridLine :: Diagrams.Diagram Diagrams.Cairo Diagrams.R2
gridLine =
  Diagrams.centerX $
  Diagrams.lwO 1 $
  Diagrams.strokeLine $
  Diagrams.lineFromVertices [Diagrams.p2 (0,0),Diagrams.p2 (1,0)]
