{-# LANGUAGE PatternSynonyms #-}
module Util where

import Foreign
import Control.Monad.IO.Class (MonadIO, liftIO)
import Graphics.GL

overPtr :: (MonadIO m, Storable a) => (Ptr a -> IO b) -> m a
overPtr f =
  liftIO (alloca (\p ->
                    do _ <- f p
                       peek p))

glGetErrors :: IO ()
glGetErrors = do
  code <- glGetError
  case code of
    GL_NO_ERROR -> return ()
    e -> do
      case e of
        GL_INVALID_ENUM -> putStrLn "* Invalid Enum"
        GL_INVALID_VALUE -> putStrLn "* Invalid Value"
        GL_INVALID_OPERATION -> putStrLn "* Invalid Operation"
        GL_INVALID_FRAMEBUFFER_OPERATION -> putStrLn "* Invalid Framebuffer Operation"
        GL_OUT_OF_MEMORY -> putStrLn "* OOM"
        GL_STACK_UNDERFLOW -> putStrLn "* Stack underflow"
        GL_STACK_OVERFLOW -> putStrLn "* Stack overflow"
        _ -> return ()
      glGetErrors
