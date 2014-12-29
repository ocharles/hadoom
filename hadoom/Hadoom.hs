{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom where

import Control.Applicative
import Control.Exception (finally)
import Control.Lens hiding (indices)
import Control.Monad (forM, forM_, replicateM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.State.Strict (evalStateT, execStateT)
import Data.Distributive (distribute)
import Data.Foldable (any, traverse_)
import Data.Function (fix)
import Data.Tuple (swap)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import Foreign
import Foreign.C
import Graphics.GL
import Light
import Linear as L
import Material
import Physics
import Prelude hiding (any, floor)
import Sector
import Shader
import Unsafe.Coerce (unsafeCoerce)
import Util
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified FRP
import qualified Quine.Debug as Quine
import qualified SDL

import qualified Level as NewLevelDef

screenWidth, screenHeight :: GLsizei
(screenWidth,screenHeight) = (800,600)

aCol :: V.Vector (V2 CFloat)
aCol =
  [V2 (-0.3)
      (-0.3)
  ,V2 (-0.3) 0.3
  ,V2 0.3 0.3
  ,V2 0.3 (-0.3)]

col1, col2, col3, col4 :: V.Vector (V2 CFloat)
col1 = V.map (+ V2 2.5 2.5) aCol
col2 = V.map (+ V2 2.5 (-2.5)) aCol
col3 = V.map (+ V2 (-2.5) 2.5) aCol
col4 =
  V.map (+ V2 (-2.5)
              (-2.5))
        aCol

room :: V.Vector (V2 CFloat)
room =
  V.foldl' (flip makeSimple)
           [V2 (-25)
               (-25)
           ,V2 25 (-25)
           ,V2 25 25
           ,V2 (-25) 25]
           [] --col1] -- ,col2,col3,col4]

--------------------------------------------------------------------------------
data RenderData =
  RenderData {_lightFBO :: GLuint
             ,_sectors :: [Sector]
             ,_lightTextures :: V.Vector (GLTextureObject,GLTextureObject)
             ,_nullVao :: GLuint
             }

makeClassy ''RenderData

-- Shaders can be refreshed during runtime, while the above data is immutable.
data Shaders = Shaders
    { _shadowShader :: GLProgram
    ,_sceneShader :: GLProgram
    ,_satProgram :: GLProgram
    ,_spotlightIndex :: GLuint
    ,_pointIndex :: GLuint
    ,_lightsUBO :: GLuint
    ,_reduceByAverage :: GLProgram
    }

makeClassy ''Shaders

--------------------------------------------------------------------------------
hadoom :: [Sector] -> SDL.Window -> IO b
hadoom sectors win =
  do tstart <- getCurrentTime
     static <- loadRenderData
     NewLevelDef.GLWorld drawWorld <- NewLevelDef.compile NewLevelDef.testWorld
     runReaderT
       (do initialShaders <- liftIO reloadShaders
           evalStateT
             (fix (step drawWorld)
                  (scene,tstart))
             initialShaders)
       static
  where step drawWorld again (w,currentTime) =
          do newTime <- liftIO getCurrentTime
             let frameTime = newTime `diffUTCTime` currentTime
             events <- liftIO (unfoldM SDL.pollEvent)
             when (reloadShadersRequested events)
                  (shaders <~ liftIO reloadShaders)
             let (Scene viewMat lights,w') =
                   runIdentity
                     (FRP.stepWire w
                                   (realToFrac frameTime)
                                   events)
             do s <- use sceneShader
                setUniform
                  s
                  "view"
                  (distribute
                     (fromMaybe (error "Failed to invert view matrix")
                                (inv44 viewMat)))
             -- lights' <- renderLightDepthTextures (V.take 10 lights)
             -- renderFromCamera lights'
             do glBindFramebuffer GL_FRAMEBUFFER 0
                glViewport 0 0 screenWidth screenHeight
                glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
                liftIO drawWorld
             SDL.glSwapWindow win
             again (w',newTime)
        reloadShadersRequested = (`hasScancode` SDL.ScancodeR)
        reloadShaders =
          do liftIO (putStrLn "Loading shaders")
             shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                               "shaders/fragment/solid-white.glsl"
             shadowShader <- createShaderProgram "shaders/vertex/shadow.glsl"
                                                 "shaders/fragment/depth.glsl"
             satShader <- createShaderProgram "shaders/vertex/sat.glsl" "shaders/fragment/sat.glsl"
             reduce <- createShaderProgram "shaders/vertex/sat.glsl"
                                           "shaders/fragment/subtract-mean.glsl"
             spotlightIndex <- getSubroutineIndex shaderProg "spotlight"
             pointIndex <- getSubroutineIndex shaderProg "omni"
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
                setUniform shadowShader "depthP" lPerspective
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
             lightsUBO <- overPtr (glGenBuffers 1)
             glBindBuffer GL_UNIFORM_BUFFER lightsUBO
             blockIndex <- getUniformBlockIndex shaderProg "Light"
             glBindBufferBase GL_UNIFORM_BUFFER blockIndex lightsUBO
             return Shaders {_sceneShader = shaderProg
                            ,_shadowShader = shadowShader
                            ,_satProgram = satShader
                            ,_spotlightIndex = spotlightIndex
                            ,_pointIndex = pointIndex
                            ,_lightsUBO = lightsUBO
                            ,_reduceByAverage = reduce}
        loadRenderData =
          do vertBlur <- createShaderProgram "shaders/vertex/gauss-vert.glsl"
                                             "shaders/fragment/gauss.glsl"
             horizBlur <- createShaderProgram "shaders/vertex/gauss-horiz.glsl"
                                              "shaders/fragment/gauss.glsl"
             lightFBO <- unGLFramebufferObject <$> genLightFramebufferObject
             lightTextures <- V.replicateM
                                10
                                ((,) <$> genLightDepthMap <*> genLightDepthMap)
             nullVaoId <- overPtr (glGenVertexArrays 1)
             let static =
                   RenderData {_sectors = sectors
                              ,_lightFBO = lightFBO
                              ,_lightTextures = lightTextures
                              ,_nullVao = nullVaoId}
             return static

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
                 Spotlight dir _ _ rotationMatrix ->
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
             joinMap (traverse_ (liftIO . drawSectorGeometry))
                     (view sectors)
        fullscreenQuad =
          do (src,dst) <- use id
             glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D dst 0
             glBindTexture GL_TEXTURE_2D src
             glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
             glDrawArrays GL_TRIANGLES 0 3
             id %= swap
        satPasses =
          do do p <- lift (use satProgram)
                glUseProgram (unGLProgram p)
                let satPass n =
                      do setUniform p
                                    "jump"
                                    (2 ^ n :: GLint)
                         fullscreenQuad
                    satPasses =
                      ceiling (logBase 2 (fromIntegral shadowMapResolution))
                setUniform p
                           "pixelSize"
                           (1.0 / fromIntegral shadowMapResolution :: GLfloat)
                forM_ [V2 1 0,V2 0 1] $
                  \basis ->
                    do setUniform p
                                  "basis"
                                  (basis :: V2 GLfloat)
                       mapM_ satPass [0 .. satPasses]
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
             liftIO . mapM_ drawSectorGeometry =<< view sectors
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
                             do glBufferData GL_UNIFORM_BUFFER
                                             (fromIntegral (sizeOf (undefined :: Light)))
                                             (castPtr ptr)
                                             GL_STREAM_DRAW))
             liftIO . mapM_ drawSectorTextured =<< view sectors

--------------------------------------------------------------------------------
withHadoom :: (SDL.Window -> IO b) -> IO b
withHadoom m =
  do _ <- SDL.initialize ([SDL.InitEverything] :: [SDL.InitFlag])
     win <- SDL.createWindow
              "Hadoom"
              SDL.defaultWindow {SDL.windowSize =
                                   V2 800 600
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
testHadoom :: [(Double,Double)] -> FilePath -> IO ()
testHadoom vertices wallTexture =
  withHadoom
    (\w ->
       do test <- Material <$>
                  loadTexture wallTexture SRGB <*>
                  loadTexture "flat.jpg" Linear
          let x =
                IM.fromList
                  (zip [0 ..]
                       (map (\(vx,vy) ->
                               realToFrac <$>
                               V2 (vx * 10)
                                  (vy * 10))
                            vertices))
          print x
          sector <- buildSector
                      Blueprint {blueprintVertices = x
                                ,blueprintWalls =
                                   V.fromList
                                     (let i =
                                            [0 .. length vertices - 1]
                                      in zip i (tail i ++ i))
                                ,blueprintFloor = 0
                                ,blueprintCeiling = 3
                                ,blueprintFloorMaterial = test
                                ,blueprintCeilingMaterial = test
                                ,blueprintWallMaterial = test}
          hadoom [sector] w)

--------------------------------------------------------------------------------
playHadoom :: IO ()
playHadoom =
  withHadoom
    (\w ->
       do wall1 <- Material <$>
                   loadTexture "stonework-diffuse.png" SRGB <*>
                   loadTexture "stonework-normals.png" Linear
          ceiling <- Material <$>
                     loadTexture "CrustyConcrete-ColorMap.jpg" SRGB <*>
                     loadTexture "CrustyConcrete-NormalMap.jpg" Linear
          floor <- Material <$>
                   loadTexture "tiles.png" SRGB <*>
                   loadTexture "tiles-normals.png" Linear
          sectorLargeRoom <- buildSector
                               Blueprint {blueprintVertices =
                                            IM.fromList
                                              (zip [0 ..]
                                                   (V.toList room))
                                         ,blueprintWalls =
                                            --[(0,1),(13,14),(14,27),(27,0)]
                                            [(0, 1), (1, 2), (2, 3), (3, 0)]
                                             --[]
                                         ,blueprintFloor = 0
                                         ,blueprintCeiling = 3
                                         ,blueprintFloorMaterial = floor
                                         ,blueprintCeilingMaterial = ceiling
                                         ,blueprintWallMaterial = wall1}
          [sectorCol1,sectorCol2,sectorCol3,sectorCol4] <- forM [col1
                                                                ,col2
                                                                ,col3
                                                                ,col4]
                                                                (\col ->
                                                                   buildSector
                                                                     Blueprint {blueprintVertices =
                                                                                  IM.fromList
                                                                                    (zip [0 ..]
                                                                                         (V.toList col))
                                                                               ,blueprintWalls =
                                                                                  [(0
                                                                                   ,1)
                                                                                  ,(1
                                                                                   ,2)
                                                                                  ,(2
                                                                                   ,3)
                                                                                  ,(3
                                                                                   ,0)]
                                                                               ,blueprintFloor =
                                                                                  (-2)
                                                                               ,blueprintCeiling = 3
                                                                               ,blueprintFloorMaterial = floor
                                                                               ,blueprintCeilingMaterial = ceiling
                                                                               ,blueprintWallMaterial = wall1})
          --hadoom [sectorCol1,sectorCol2,sectorCol3,sectorCol4,sectorLargeRoom] w)
          hadoom [sectorLargeRoom, sectorCol2] w)

--------------------------------------------------------------------------------
joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap = (=<<)
