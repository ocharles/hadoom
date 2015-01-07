module Hadoom.Editor.Mode.Default (defaultMode) where

import Hadoom.Editor.GUI
import Hadoom.Editor.SectorBuilder
import Hadoom.Editor.Render
import Reactive.Banana
import Reactive.Banana.Frameworks

defaultMode :: Frameworks t => HadoomGUI -> SectorBuilder -> Moment t (Behavior t Diagram)