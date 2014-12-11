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
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldM)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Distributive (distribute)
import Data.Foldable (traverse_)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import Foreign
import Foreign.C
import Graphics.GL
import Light
import Linear as L
import Material
import Physics
import Prelude hiding (any, floor, ceiling)
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
           [V2 (-5)
               (-5)
           ,V2 5 (-5)
           ,V2 5 5
           ,V2 (-5) 5]
           [col1,col2,col3,col4]

--------------------------------------------------------------------------------
data RenderData =
  RenderData {_shadowShader :: GLuint
             ,_lightFBO :: GLuint
             ,_sectors :: [Sector]
             ,_lightTextures :: V.Vector GLTextureObject}

makeClassy ''RenderData

--------------------------------------------------------------------------------
hadoom :: [Sector] -> SDL.Window -> IO b
hadoom sectors win =
  do shaderProg <- unGLProgram <$>
                   createShaderProgram "shaders/vertex/projection-model.glsl"
                                       "shaders/fragment/solid-white.glsl"
     spotlightIndex <- withCString "spotlight"
                                   (glGetSubroutineIndex shaderProg GL_FRAGMENT_SHADER)
     pointIndex <- withCString "omni"
                               (glGetSubroutineIndex shaderProg GL_FRAGMENT_SHADER)
     shadowShader <- unGLProgram <$>
                     createShaderProgram "shaders/vertex/shadow.glsl" "shaders/fragment/depth.glsl"
     let perspectiveMat :: M44 GLfloat
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
     with perspectiveMat
          (\ptr ->
             do loc1 <- withCString "projection"
                                    (glGetUniformLocation shaderProg)
                glUseProgram shaderProg
                glUniformMatrix4fv loc1
                                   1
                                   0
                                   (castPtr ptr))
     with lPerspective
          (\ptr ->
             do loc <- withCString "lightProjection"
                                   (glGetUniformLocation shaderProg)
                glUseProgram shaderProg
                glUniformMatrix4fv loc
                                   1
                                   0
                                   (castPtr ptr)
                loc1 <- withCString "depthP"
                                    (glGetUniformLocation shadowShader)
                glUseProgram shadowShader
                glUniformMatrix4fv loc1
                                   1
                                   0
                                   (castPtr ptr))
     let bias =
           [0.5,0,0,0,0,0.5,0,0,0,0,0.5,0,0.5,0.5,0.5,1]
     SV.unsafeWith
       bias
       (\ptr ->
          do loc1 <- withCString "bias"
                                 (glGetUniformLocation shaderProg)
             glUseProgram shaderProg
             glUniformMatrix4fv loc1 1 0 ptr)
     do loc <- withCString "tex"
                           (glGetUniformLocation shaderProg)
        glUniform1i loc 0
     do loc <- withCString "depthMap"
                           (glGetUniformLocation shaderProg)
        glUniform1i loc 1
     do loc <- withCString "nmap"
                           (glGetUniformLocation shaderProg)
        glUniform1i loc 2
     shaderId <- unsafeCoerce shaderProg
     blockIndex <- withCString "Light"
                               (glGetUniformBlockIndex shaderId)
     lightsUBO <- overPtr (glGenBuffers 1)
     glBindBuffer GL_UNIFORM_BUFFER lightsUBO
     glBindBufferBase GL_UNIFORM_BUFFER blockIndex lightsUBO
     tstart <- getCurrentTime
     lightFBO <- unGLFramebufferObject <$> genLightFramebufferObject
     lightTextures <- V.replicateM 10 genLightDepthMap
     let rd =
           RenderData {_shadowShader = shadowShader
                      ,_lightFBO = lightFBO
                      ,_sectors = sectors
                      ,_lightTextures = lightTextures}
     runReaderT
       (fix (\again (w,currentTime) ->
               do newTime <- liftIO getCurrentTime
                  let frameTime = newTime `diffUTCTime` currentTime
                  events <- liftIO (unfoldM SDL.pollEvent)
                  let (Scene viewMat lights,w') =
                        runIdentity
                          (FRP.stepWire w
                                        (realToFrac frameTime)
                                        events)
                  liftIO (with (distribute
                                  (fromMaybe (error "Failed to invert view matrix")
                                             (inv44 viewMat)))
                               (\ptr ->
                                  do loc1 <- withCString "view"
                                                         (glGetUniformLocation shaderProg)
                                     glUseProgram shaderProg
                                     glUniformMatrix4fv loc1
                                                        1
                                                        0
                                                        (castPtr (ptr :: Ptr (M44 CFloat)))))
                  lights' <- renderLightDepthTextures lights
                  renderFromCamera shaderProg lights' pointIndex spotlightIndex lightsUBO
                  SDL.glSwapWindow win
                  again (w',newTime))
            (scene,tstart))
       rd

--------------------------------------------------------------------------------
renderLightDepthTextures :: (Applicative m, MonadIO m, MonadReader r m, HasRenderData r)
                         => V.Vector Light
                         -> m (V.Vector (Light,GLuint,M44 CFloat))
renderLightDepthTextures lights =
  do glDisable GL_BLEND
     glDepthMask GL_TRUE
     glDepthFunc GL_LEQUAL
     glUseProgram =<< view shadowShader
     glBindFramebuffer GL_DRAW_FRAMEBUFFER =<<
       view lightFBO
     glViewport 0 0 shadowMapResolution shadowMapResolution
     glEnable GL_CULL_FACE
     glCullFace GL_BACK
     lightsWithTextures <- V.zip lights <$> view lightTextures
     V.mapM (\(l,GLTextureObject t) ->
               renderLightDepthTexture l t)
            lightsWithTextures

--------------------------------------------------------------------------------
renderLightDepthTexture :: (Applicative m, MonadIO m, MonadReader r m, HasRenderData r)
                        => Light
                        -> GLuint
                        -> m (Light,GLuint,M44 CFloat)
renderLightDepthTexture l t =
  do case lightShape l of
       Spotlight dir _ _ rotationMatrix ->
         do glFramebufferTexture2D GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D t 0
            liftIO (withArray [GL_COLOR_ATTACHMENT0]
                              (glDrawBuffers 1))
            glClear (GL_DEPTH_BUFFER_BIT .|. GL_COLOR_BUFFER_BIT)
            let v =
                  m33_to_m44 rotationMatrix !*!
                  mkTransformation 0
                                   (negate (lightPos l))
            shadowShaderId <- view shadowShader
            liftIO (with (distribute v)
                         (\ptr ->
                            do loc <- withCString "depthV"
                                                  (glGetUniformLocation shadowShaderId)
                               glUniformMatrix4fv loc
                                                  1
                                                  0
                                                  (castPtr (ptr :: Ptr (M44 CFloat)))))
            joinMap (traverse_ (\Sector{..} ->
                                  liftIO (do sectorDrawWalls
                                             sectorDrawFloor
                                             sectorDrawCeiling)))
                    (view sectors)
            return (l,t,distribute v)
       Omni ->
         let v =
               mkTransformation 0
                                (negate (lightPos l))
         in return (l,t,distribute v)

--------------------------------------------------------------------------------
renderFromCamera :: (MonadIO m, MonadReader r m, HasRenderData r)
                 => GLuint
                 -> V.Vector (Light,GLuint,M44 CFloat)
                 -> GLuint
                 -> GLuint
                 -> GLuint
                 -> m ()
renderFromCamera shaderProg lights pointIndex spotlightIndex lightsUBO =
  do activateDefaultFramebuffer
     enableBackfaceCulling
     renderDepthBuffer
     prepareMultipassRendering
     glUseProgram shaderProg
     V.mapM_ (\(l,t,v) -> renderPass l t v) lights
  where activateDefaultFramebuffer =
          do glBindFramebuffer GL_FRAMEBUFFER 0
             glViewport 0 0 screenWidth screenHeight
        enableBackfaceCulling =
          do glEnable GL_CULL_FACE
             glCullFace GL_BACK
        renderDepthBuffer =
          do glClear GL_DEPTH_BUFFER_BIT
             glUseProgram shaderProg
             liftIO . mapM_ drawSectorTextured =<< view sectors
        prepareMultipassRendering =
          do glEnable GL_BLEND
             glBlendFunc GL_ONE GL_ONE
             glClear GL_COLOR_BUFFER_BIT
             glDepthFunc GL_EQUAL
             glDepthMask GL_FALSE
        renderPass l t v =
          do liftIO (with v
                          (\ptr ->
                             do loc <- withCString "camV"
                                                   (glGetUniformLocation shaderProg)
                                glUniformMatrix4fv loc
                                                   1
                                                   0
                                                   (castPtr (ptr :: Ptr (M44 CFloat)))))
             let i =
                   case lightShape l of
                     Omni -> pointIndex
                     Spotlight{} -> spotlightIndex
             liftIO (with i (glUniformSubroutinesuiv GL_FRAGMENT_SHADER 1))
             glActiveTexture GL_TEXTURE1
             glBindTexture GL_TEXTURE_2D t
             liftIO (with l
                          (\ptr ->
                             do glBindBuffer GL_UNIFORM_BUFFER lightsUBO
                                glBufferData GL_UNIFORM_BUFFER
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
                                            [(0,1),(13,14),(14,27),(27,0)]
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
          hadoom [sectorLargeRoom,sectorCol1,sectorCol2,sectorCol3,sectorCol4] w)

joinMap :: Monad m => (a -> m b) -> m a -> m b
joinMap = (=<<)
