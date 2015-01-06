module Reactive.Banana.GTK where

import BasePrelude
import Control.Monad.Trans (liftIO)
import Linear
import Linear.Affine
import qualified Graphics.UI.Gtk as GTK
import qualified Reactive.Banana as RB
import qualified Reactive.Banana.Frameworks as RB

registerDestroy :: (RB.Frameworks t,GTK.WidgetClass w)
                => w -> RB.Moment t (RB.Event t ())
registerDestroy widget =
  RB.fromAddHandler
    (RB.AddHandler
       (\h ->
          fmap GTK.signalDisconnect (GTK.on widget GTK.objectDestroy (liftIO (h ())))))

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

withEvent :: (GTK.GObjectClass obj,RB.MonadIO m)
          => (t -> IO (GTK.ConnectId obj))
          -> ((a -> m ()) -> t)
          -> RB.AddHandler a
withEvent f m =
  RB.AddHandler
    (\h ->
       fmap GTK.signalDisconnect (f (m (liftIO . h))))

registerToolButtonClicked b =
  RB.fromAddHandler
    (RB.AddHandler
       (\h ->
          fmap GTK.signalDisconnect
               (GTK.onToolButtonClicked b
                                        (h ()))))

registerKeyPressed w =
  RB.fromAddHandler
    (RB.AddHandler
       (\h ->
          fmap GTK.signalDisconnect
               (GTK.on w
                       GTK.keyReleaseEvent
                       (do kv <- GTK.eventKeyVal
                           True <$
                             (liftIO (h kv)))))) -- TODO This should return False if we're not interested in the event
