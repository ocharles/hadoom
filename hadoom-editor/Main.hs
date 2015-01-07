{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import BasePrelude
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Linear
import qualified Diagrams.Backend.Cairo.Internal as Cairo
import qualified Diagrams.Prelude as D
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

import Hadoom.Editor
import Hadoom.Editor.GUI

main :: IO ()
main =
  do _ <- GTK.initGUI
     builder <- GTK.builderNew
     GTK.builderAddFromFile builder "hadoom-editor/editor.glade"
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
            pure da <*>
            pure (outputSize / 50) <*>
            GTK.builderGetObject builder GTK.castToToolButton "toolbutton1"
     RB.compile (editorNetwork gui) >>=
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
