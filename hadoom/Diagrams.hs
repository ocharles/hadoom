module Diagrams where

import Control.Monad.Trans (liftIO)
import Graphics.UI.Gtk
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import Diagrams.Prelude

blockDisplayingFigure :: Diagram Cairo R2 -> IO ()
blockDisplayingFigure figure =
  do initGUI
     window <- windowNew
     canvas <- drawingAreaNew
     canvas `on`
       sizeRequest (return (Requisition 256 256))
     set window [containerBorderWidth := 10,containerChild := canvas]
     canvas `on`
       exposeEvent (renderFigure figure)
     onDestroy window mainQuit
     widgetShowAll window
     mainGUI

renderFigure :: Diagram Cairo R2 -> EventM EExpose Bool
renderFigure figure =
  do win <- eventWindow
     liftIO (renderToGtk win
                         (toGtkCoords (scale 3 figure)))
     return True
