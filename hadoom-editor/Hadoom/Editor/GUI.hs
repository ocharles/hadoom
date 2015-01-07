module Hadoom.Editor.GUI (HadoomGUI(..)) where

import BasePrelude
import Hadoom.Editor.Render (Diagram)
import Linear
import qualified Graphics.UI.Gtk as GTK

data HadoomGUI =
  HadoomGUI {appWindow :: GTK.Window
            ,outRef :: IORef Diagram
            ,guiMap :: GTK.DrawingArea
            ,mapExtents :: V2 Double
            ,playHadoomButton :: GTK.ToolButton}
