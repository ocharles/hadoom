{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom where

import Control.Applicative
import Control.Lens hiding (indices)
import Control.Monad (forM, forM_, guard)
import Control.Monad.Loops (unfoldM)
import Data.Distributive (distribute)
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
import qualified SDL.Raw.Basic as Raw
import qualified SDL.Raw.Enum as Raw
import qualified SDL.Raw.Event as Raw
import qualified SDL.Raw.Types as Raw

(screenWidth,screenHeight) = (800,600)

col1, col2, col3, col4 :: V.Vector (V2 CFloat)
col1 =
  V.map (+ V2 (20) 20)
        [V2 (-2)
            (-2)
        ,V2 (-2) 2
        ,V2 2 2
        ,V2 2 (-2)]
col2 =
  V.map (+ V2 (20)
              (-20))
        [V2 (-2)
            (-2)
        ,V2 (-2) 2
        ,V2 2 2
        ,V2 2 (-2)]
col3 =
  V.map (+ V2 (-20) 20)
        [V2 (-2)
            (-2)
        ,V2 (-2) 2
        ,V2 2 2
        ,V2 2 (-2)]
col4 =
  V.map (+ V2 (-20)
              (-20))
        [V2 (-2)
            (-2)
        ,V2 (-2) 2
        ,V2 2 2
        ,V2 2 (-2)]

room =
  V.foldl' (flip makeSimple)
           [V2 (-50)
               (-50)
           ,V2 50 (-50)
           ,V2 50 50
           ,V2 (-50) 50]
           [col1,col2,col3,col4]

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
     m win

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
                       (map (\(x,y) ->
                               realToFrac <$>
                               V2 (x * 10)
                                  (y * 10))
                            vertices))
          print x
          sector <- buildSector
                      Blueprint {blueprintVertices = x
                                ,blueprintWalls =
                                   V.fromList
                                     (let i =
                                            [0 .. length vertices - 1]
                                      in zip i (tail i ++ i))
                                ,blueprintFloor =
                                   (-2)
                                ,blueprintCeiling = 20
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
          wall2 <- Material <$>
                   loadTexture "stonework-015_d100.png" SRGB <*>
                   loadTexture "stonework-015_b020-p050.png" Linear
          ceiling <- Material <$>
                     loadTexture "CrustyConcrete-ColorMap.jpg" SRGB <*>
                     loadTexture "CrustyConcrete-NormalMap.jpg" Linear
          floor <- Material <$>
                   loadTexture "tiles.png" SRGB <*>
                   loadTexture "tiles-normals.png" Linear
          test <- Material <$>
                  loadTexture "test-texture.jpg" SRGB <*>
                  loadTexture "debug-normals.png" Linear
          flat <- Material <$>
                  loadTexture "white.jpg" SRGB <*>
                  loadTexture "flat.jpg" Linear
          gamma <- Material <$>
                   loadTexture "gamma.png" SRGB <*>
                   loadTexture "flat.jpg" Linear
          sectorLargeRoom <- buildSector
                               Blueprint {blueprintVertices =
                                            IM.fromList
                                              (zip [0 ..]
                                                   (V.toList room))
                                         ,blueprintWalls =
                                            [(0,1),(13,14),(14,27),(27,0)]
                                         ,blueprintFloor =
                                            (-2)
                                         ,blueprintCeiling = 20
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
                                                                               ,blueprintCeiling = 20
                                                                               ,blueprintFloorMaterial = floor
                                                                               ,blueprintCeilingMaterial = ceiling
                                                                               ,blueprintWallMaterial = wall1})
          hadoom [sectorLargeRoom,sectorCol1,sectorCol2,sectorCol3,sectorCol4] w)


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
     glDepthFunc GL_LEQUAL
     shaderId <- unsafeCoerce shaderProg
     blockIndex <- withCString "Light"
                               (glGetUniformBlockIndex shaderId)
     lightsUBO <- overPtr (glGenBuffers 1)
     glBindBuffer GL_UNIFORM_BUFFER lightsUBO
     glBindBufferBase GL_UNIFORM_BUFFER blockIndex lightsUBO
     tstart <- getCurrentTime
     lightFBO <- unGLFramebufferObject <$> genLightFramebufferObject
     lightTextures <- V.replicateM 10 genLightDepthMap
     fix (\again (w,currentTime) ->
            do newTime <- getCurrentTime
               let frameTime = newTime `diffUTCTime` currentTime
               events <- unfoldM SDL.pollEvent
               let (Scene viewMat lights,w') =
                     runIdentity
                       (FRP.stepWire w
                                     (realToFrac frameTime)
                                     events)
               with (distribute
                       (fromMaybe (error "Failed to invert view matrix")
                                  (inv44 viewMat)))
                    (\ptr ->
                       do loc1 <- withCString "view"
                                              (glGetUniformLocation shaderProg)
                          glUseProgram shaderProg
                          glUniformMatrix4fv loc1
                                             1
                                             0
                                             (castPtr (ptr :: Ptr (M44 CFloat))))
               glDisable GL_BLEND
               glDepthMask GL_TRUE
               glDepthFunc GL_LEQUAL
               glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
               glUseProgram shadowShader
               glBindFramebuffer GL_FRAMEBUFFER lightFBO
               glViewport 0 0 shadowMapResolution shadowMapResolution
               glDisable GL_CULL_FACE
               lights' <- flip V.mapM
                               (V.zip lights lightTextures)
                               (\(l,GLTextureObject t) ->
                                  do case lightShape l of
                                       Spotlight dir _ _ rotationMatrix ->
                                         do glFramebufferTexture2D GL_FRAMEBUFFER
                                                                   GL_DEPTH_ATTACHMENT
                                                                   GL_TEXTURE_2D
                                                                   t
                                                                   0
                                            glClear GL_DEPTH_BUFFER_BIT
                                            let v =
                                                  m33_to_m44 rotationMatrix !*!
                                                  mkTransformation 0
                                                                   (negate (lightPos l))
                                            with (distribute v)
                                                 (\ptr ->
                                                    do loc <- withCString "depthV"
                                                                          (glGetUniformLocation shadowShader)
                                                       glUniformMatrix4fv loc
                                                                          1
                                                                          0
                                                                          (castPtr (ptr :: Ptr (M44 CFloat))))
                                            forM_ sectors
                                                  (\s ->
                                                     case s of
                                                       Sector{..} -> sectorDrawWalls >>
                                                                       sectorDrawFloor)
                                            return (l,t,distribute v)
                                       Omni ->
                                         let v =
                                               mkTransformation 0
                                                                (negate (lightPos l))
                                         in return (l,t,distribute v))
               glBindFramebuffer GL_FRAMEBUFFER 0
               -- glEnable GL_CULL_FACE
               -- glCullFace GL_BACK
               glViewport 0 0 screenWidth screenHeight
               glClear GL_DEPTH_BUFFER_BIT
               glUseProgram shaderProg
               mapM_ drawSectorTextured sectors
               glEnable GL_BLEND
               glBlendFunc GL_ONE GL_ONE
               glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
               glClear GL_COLOR_BUFFER_BIT
               glDepthFunc GL_EQUAL
               glDepthMask GL_FALSE
               glUseProgram shaderProg
               flip V.mapM_
                    lights'
                    (\(l,t,v) ->
                       do with v
                               (\ptr ->
                                  do glUseProgram shaderProg
                                     loc <- withCString "camV"
                                                        (glGetUniformLocation shaderProg)
                                     glUniformMatrix4fv loc
                                                        1
                                                        0
                                                        (castPtr (ptr :: Ptr (M44 CFloat))))
                          let i =
                                case lightShape l of
                                  Omni -> pointIndex
                                  Spotlight{} -> spotlightIndex
                          with i (glUniformSubroutinesuiv GL_FRAGMENT_SHADER 1)
                          glActiveTexture GL_TEXTURE1
                          glBindTexture GL_TEXTURE_2D t
                          with l
                               (\ptr ->
                                  do glBindBuffer GL_UNIFORM_BUFFER lightsUBO
                                     glBufferData GL_UNIFORM_BUFFER
                                                  (fromIntegral (sizeOf (undefined :: Light)))
                                                  (castPtr ptr)
                                                  GL_STREAM_DRAW)
                          -- GL.polygonMode $= (GL.Line, GL.Line)
                          mapM_ drawSectorTextured sectors)
               SDL.glSwapWindow win
               again (w',newTime))
         (scene,tstart)
