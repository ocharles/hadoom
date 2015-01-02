{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Hadoom where

import Control.Exception (finally)
import Control.Lens hiding (indices)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT)
import Control.Wire hiding (when)
import Data.Distributive (distribute)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Foreign
import Foreign.C
import Graphics.GL
import Hadoom.GL.World
import Hadoom.World
import Hadoom.WorldBSP
import Light
import Linear as L
import Material
import Physics
import Prelude hiding (any, floor, (.), id)
import Shader
import TestWorld
import Util
import qualified Data.Vector as V
import qualified Quine.Debug as Quine
import qualified SDL

screenWidth, screenHeight :: GLsizei
(screenWidth,screenHeight) = (1024,768)

--------------------------------------------------------------------------------
data RenderData =
  RenderData {_lightFBO :: GLuint
             ,_world :: CompiledWorld
             ,_lightTextures :: V.Vector (GLTextureObject,GLTextureObject)
             ,_nullVao :: GLuint}

makeClassy ''RenderData

-- Shaders can be refreshed during runtime, while the above data is immutable.
data Shaders =
  Shaders {_shadowShader :: GLProgram
          ,_sceneShader :: GLProgram
          ,_satProgram :: GLProgram
          ,_spotlightIndex :: GLuint
          ,_pointIndex :: GLuint
          ,_lightsUBO :: GLuint
          ,_reduceByAverage :: GLProgram}

makeClassy ''Shaders

--------------------------------------------------------------------------------
hadoom :: PWorld 'TWorld -> SDL.Window -> IO b
hadoom world win =
  do static <- loadRenderData
     runReaderT
       (do initialShaders <- liftIO reloadShaders
           evalStateT (fix step (scene (compileBSP world),clockSession_))
                      initialShaders)
       static
  where step again (w,sess) =
          do (delta,s') <- stepSession sess
             events <- liftIO (unfoldM SDL.pollEvent)
             when (reloadShadersRequested events)
                  (shaders <~ liftIO reloadShaders)
             let (out,w') =
                   runIdentity (stepWire w delta (Right events))
                 (Scene viewMat lights) =
                   case out of
                     Right x -> x
                     Left () ->
                       error "Unpossible"
             do s <- use sceneShader
                setUniform
                  s
                  "view"
                  (distribute
                     (fromMaybe (error "Failed to invert view matrix")
                                (inv44 viewMat)))
             lights' <- renderLightDepthTextures (V.take 10 lights)
             renderFromCamera lights'
             SDL.glSwapWindow win
             again (w',s')
        reloadShadersRequested = (`hasScancode` SDL.ScancodeR)
        reloadShaders =
          do liftIO (putStrLn "Loading shaders")
             shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                               "shaders/fragment/solid-white.glsl"
             shadowShader_ <- createShaderProgram "shaders/vertex/shadow.glsl"
                                                  "shaders/fragment/depth.glsl"
             satShader <- createShaderProgram "shaders/vertex/sat.glsl" "shaders/fragment/sat.glsl"
             reduce <- createShaderProgram "shaders/vertex/sat.glsl"
                                           "shaders/fragment/subtract-mean.glsl"
             spotlightIndex_ <- getSubroutineIndex shaderProg "spotlight"
             pointIndex_ <- getSubroutineIndex shaderProg "omni"
             do let perspectiveMat :: M44 GLfloat
                    perspectiveMat =
                      perspective (pi / 180 * 40)
                                  (800 / 600)
                                  1
                                  100
                    lPerspective :: M44 GLfloat
                    lPerspective =
                      perspective 2.27
                                  (4 / 3)
                                  1
                                  100
                setUniform shaderProg "projection" perspectiveMat
                setUniform shaderProg "lightProjection" lPerspective
                setUniform shadowShader_ "depthP" lPerspective
                let bias =
                      V4 (V4 0.5 0 0 0)
                         (V4 0 0.5 0 0)
                         (V4 0 0 0.5 0)
                         (V4 0.5 0.5 0.5 1)
                setUniform shaderProg
                           "bias"
                           (bias :: M44 CFloat)
                setUniform shaderProg
                           "tex"
                           (0 :: Int32)
                setUniform shaderProg
                           "depthMap"
                           (1 :: Int32)
                setUniform shaderProg
                           "nmap"
                           (2 :: Int32)
             lightsUBO_ <- overPtr (glGenBuffers 1)
             glBindBuffer GL_UNIFORM_BUFFER lightsUBO_
             blockIndex <- getUniformBlockIndex shaderProg "Light"
             glBindBufferBase GL_UNIFORM_BUFFER blockIndex lightsUBO_
             return Shaders {_sceneShader = shaderProg
                            ,_shadowShader = shadowShader_
                            ,_satProgram = satShader
                            ,_spotlightIndex = spotlightIndex_
                            ,_pointIndex = pointIndex_
                            ,_lightsUBO = lightsUBO_
                            ,_reduceByAverage = reduce}
        loadRenderData =
          RenderData <$>
          (unGLFramebufferObject <$> genLightFramebufferObject) <*>
          (compile world >>= \(GLWorld w) -> return w) <*>
          V.replicateM 10
                       ((,) <$> genLightDepthMap <*> genLightDepthMap) <*>
          overPtr (glGenVertexArrays 1)

--------------------------------------------------------------------------------
renderLightDepthTextures :: (Applicative m,MonadIO m,MonadReader r m,HasRenderData r,HasShaders s,MonadState s m)
                         => V.Vector Light
                         -> m (V.Vector (Light,GLuint,M44 CFloat))
renderLightDepthTextures lights =
  do glDisable GL_BLEND
     glDepthMask GL_TRUE
     glDepthFunc GL_LEQUAL
     glUseProgram . unGLProgram =<< use shadowShader
     glBindFramebuffer GL_DRAW_FRAMEBUFFER =<<
       view lightFBO
     glViewport 0 0 shadowMapResolution shadowMapResolution
     glEnable GL_CULL_FACE
     glCullFace GL_BACK
     lightsWithTextures <- V.zip lights <$> view lightTextures
     V.mapM (\(l,(GLTextureObject t1, GLTextureObject t2)) ->
               renderLightDepthTexture l t1 t2)
            lightsWithTextures

--------------------------------------------------------------------------------
renderLightDepthTexture :: (Applicative m,MonadIO m,MonadReader r m,HasRenderData r,HasShaders s,MonadState s m)
                        => Light
                        -> GLuint
                        -> GLuint
                        -> m (Light,GLuint,M44 CFloat)
renderLightDepthTexture l t1 t2 =
  do glEnable GL_DEPTH_TEST
     (v,t') <- case lightShape l of
                 Spotlight _ _ _ rotationMatrix ->
                   let v =
                         distribute
                           (m33_to_m44 rotationMatrix !*!
                            mkTransformation 0
                                             (negate (lightPos l)))
                   in do renderDepthTexture v
                         t' <- computeSummedAreaTable
                         return (v,t')
                 Omni ->
                   return (distribute
                             (mkTransformation 0
                                               (negate (lightPos l)))
                          ,t1)
     return (l,t',v)
  where renderDepthTexture v =
          do glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D t1 0
             liftIO (withArray [GL_COLOR_ATTACHMENT0]
                               (glDrawBuffers 1))
             glClearColor 1 1 1 1
             glClear (GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT)
             glClearColor 0 0 0 0
             do s <- use shadowShader
                setUniform s "depthV" v
             liftIO . drawWorldGeometry =<< view world
        fullscreenQuad =
          do (src,dst) <- use id
             glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D dst 0
             glBindTexture GL_TEXTURE_2D src
             glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
             glDrawArrays GL_TRIANGLES 0 3
             id %= swap
        satPasses =
          do p <- lift (use satProgram)
             glUseProgram (unGLProgram p)
             let satPass i =
                   do setUniform p
                                 "jump"
                                 (2 ^ i :: GLint)
                      fullscreenQuad
                 n =
                   ceiling (logBase 2 (fromIntegral shadowMapResolution) :: Float)
             setUniform p
                        "pixelSize"
                        (1.0 / fromIntegral shadowMapResolution :: GLfloat)
             forM_ [V2 1 0,V2 0 1] $
               \satBasis ->
                 do setUniform p
                               "basis"
                               (satBasis :: V2 GLfloat)
                    mapM_ satPass [0 .. n :: Int]
        computeSummedAreaTable =
          do glBindVertexArray =<< view nullVao
             glActiveTexture GL_TEXTURE0
             (src,_) <- execStateT
                          (do sp <- lift (use sceneShader)
                              determineAverage >>=
                                setUniform sp "mean"
                              subtractAverage
                              satPasses)
                          (t1,t2)
             return src
        determineAverage :: (MonadIO m, MonadState (GLuint, GLuint) m) => m (V4 GLfloat)
        determineAverage =
          do glBindTexture GL_TEXTURE_2D =<<
               use _1
             glGenerateMipmap GL_TEXTURE_2D
             overPtr (glGetTexImage GL_TEXTURE_2D 9 GL_RGBA GL_FLOAT .
                      castPtr)
        subtractAverage =
          do p <- lift (use reduceByAverage)
             glUseProgram (unGLProgram p)
             glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
             fullscreenQuad
             glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR

--------------------------------------------------------------------------------
renderFromCamera :: (MonadIO m, MonadReader r m, HasRenderData r, HasShaders s, MonadState s m)
                 => V.Vector (Light,GLuint,M44 CFloat)
                 -> m ()
renderFromCamera lights =
  do activateDefaultFramebuffer
     enableBackfaceCulling
     renderDepthBuffer
     prepareMultipassRendering
     glUseProgram . unGLProgram =<< use sceneShader
     V.mapM_ (\(l,t,v) -> renderPass l t v) lights
  where activateDefaultFramebuffer =
          do glBindFramebuffer GL_FRAMEBUFFER 0
             glViewport 0 0 screenWidth screenHeight
        enableBackfaceCulling =
          do glEnable GL_CULL_FACE
             glCullFace GL_BACK
        renderDepthBuffer =
          do glClear GL_DEPTH_BUFFER_BIT
             glUseProgram . unGLProgram =<< use sceneShader
             liftIO . drawWorldGeometry =<< view world
        prepareMultipassRendering =
          do glEnable GL_BLEND
             glBlendFunc GL_ONE GL_ONE
             glClear GL_COLOR_BUFFER_BIT
             glDepthFunc GL_EQUAL
             glDepthMask GL_FALSE
        renderPass l t v =
          do do s <- use sceneShader
                setUniform s "camV" v
             i <- use (case lightShape l of
                        Omni -> pointIndex
                        Spotlight{} -> spotlightIndex)
             liftIO (with i (glUniformSubroutinesuiv GL_FRAGMENT_SHADER 1))
             glActiveTexture GL_TEXTURE1
             glBindTexture GL_TEXTURE_2D t
             glBindBuffer GL_UNIFORM_BUFFER =<<
               use lightsUBO
             liftIO (with l
                          (\ptr ->
                             glBufferData GL_UNIFORM_BUFFER
                                          (fromIntegral (sizeOf (undefined :: Light)))
                                          (castPtr ptr)
                                          GL_STREAM_DRAW))
             liftIO . drawWorldTextured =<< view world

--------------------------------------------------------------------------------
withGLWindow :: (SDL.Window -> IO b) -> IO b
withGLWindow m =
  do _ <- SDL.initialize ([SDL.InitEverything] :: [SDL.InitFlag])
     win <- SDL.createWindow
              "Hadoom"
              SDL.defaultWindow {SDL.windowSize =
                                   fromIntegral <$> V2 screenWidth screenHeight
                                ,SDL.windowOpenGL =
                                   Just (SDL.defaultOpenGL {SDL.glProfile =
                                                              SDL.Core SDL.Debug 3 2})}
     renderer <- SDL.createRenderer win
                                    (-1)
                                    SDL.defaultRenderer
     glClearColor 0 0 0 1
     glEnable GL_FRAMEBUFFER_SRGB
     glEnable GL_DEPTH_TEST
     Quine.installDebugHook
     SDL.setRelativeMouseMode True
     m win `finally` SDL.destroyRenderer renderer

--------------------------------------------------------------------------------
playHadoom :: IO ()
playHadoom = withGLWindow (hadoom testWorld)

--------------------------------------------------------------------------------
joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap = (=<<)
