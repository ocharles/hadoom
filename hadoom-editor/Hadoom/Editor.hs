{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor (editorNetwork, outputSize) where

import BasePrelude
import Hadoom.Editor.GUI
import Hadoom.Editor.Mode.Default (defaultMode)
import Hadoom.Editor.Render
import Hadoom.Editor.SectorBuilder
import Linear
import Reactive.Banana.GTK
import qualified Diagrams.Prelude as D
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

-- TODO
outputSize :: Num a => V2 a
outputSize = V2 30 30 ^* 50

editorNetwork :: RB.Frameworks t
              => HadoomGUI -> RB.Moment t ()
editorNetwork gui@HadoomGUI{..} =
  do mainWindowClosed <- registerDestroy appWindow
     RB.reactimate (GTK.mainQuit <$ mainWindowClosed)
     diagram <- do d <- defaultMode gui emptySectorBuilder
                   return (D.lc D.white .
                           D.lwO 1 <$>
                           (mappend <$> d <*>
                            (renderGrid <$> pure mapExtents)))
     do diagramChanged <- RB.changes diagram
        RB.reactimate'
          (fmap (\d ->
                   do writeIORef outRef d
                      GTK.widgetQueueDraw guiMap) <$>
           diagramChanged)
