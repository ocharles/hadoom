module Hadoom.Editor.Util (once, toGridCoords, toDiagramCoords) where

import BasePrelude
import Linear
import Linear.Affine
import Reactive.Banana
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D

toGridCoords :: Point V2 Double -> Point V2 Double
toGridCoords = fmap fromIntegral . fmap (round :: Double -> Int)

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

-- | Forget all but the first occurance of an 'Event'.
once :: Event t a -> Event t a
once e =
  let occured = True <$ e
  in whenE (fmap not (stepper False occured)) e
