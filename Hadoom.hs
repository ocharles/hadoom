{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (any, floor, ceiling)

import Control.Applicative
import Control.Lens hiding (indices)
import Control.Monad (forM, forM_)
import Data.Distributive (distribute)
import Data.Function (fix)
import Data.Time (getCurrentTime, diffUTCTime)
import Foreign
import Foreign.C
import Graphics.Rendering.OpenGL (($=))
import Linear as L
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Graphics.UI.SDL.Basic as SDL
import qualified Graphics.UI.SDL.Enum as SDL
import qualified Graphics.UI.SDL.Event as SDL
import qualified Graphics.UI.SDL.Types as SDL
import qualified Graphics.UI.SDL.Video as SDL

import qualified FRP

import Light
import Material
import Physics
import Sector
import Shader

(screenWidth, screenHeight) = (800, 600)

col1, col2, col3, col4 :: V.Vector (V2 CFloat)
col1 = V.map (+ V2 (20) 20) [V2 (-2) (-2), V2 (-2) 2, V2 2 2, V2 2 (-2) ]
col2 = V.map (+ V2 (20) (-20)) [V2 (-2) (-2), V2 (-2) 2, V2 2 2, V2 2 (-2) ]
col3 = V.map (+ V2 (-20) 20) [V2 (-2) (-2), V2 (-2) 2, V2 2 2, V2 2 (-2) ]
col4 = V.map (+ V2 (-20) (-20)) [V2 (-2) (-2), V2 (-2) 2, V2 2 2, V2 2 (-2) ]

room = V.foldl' (flip makeSimple)
       [V2 (-50) (-50), V2 50 (-50), V2 50 50, V2 (-50) 50 ]
       [ col1, col2, col3, col4 ]

main :: IO ()
main =
  alloca $ \winPtr ->
  alloca $ \rendererPtr -> do
    putStrLn "Initializing SDL"
    _ <- SDL.init SDL.initFlagEverything
    _ <- SDL.createWindowAndRenderer screenWidth screenHeight 0 winPtr rendererPtr
    win <- peek winPtr
    withCString "Hadoom" $ SDL.setWindowTitle win

    SDL.glSetAttribute SDL.glAttrContextProfileMask SDL.glProfileCore

    putStrLn "Setting clear color"
    GL.clearColor $= GL.Color4 0 0 0 1
    GL.get GL.errors >>= mapM_ print

    putStrLn "Enabling SRGB framebuffer"
    GL.glEnable GL.gl_FRAMEBUFFER_SRGB
    GL.get GL.errors >>= mapM_ print

    putStrLn "Loading materials"

    wall1 <- Material <$> loadTexture "stonework-diffuse.png" SRGB
                     <*> loadTexture "stonework-normals.png" Linear

    wall2 <- Material <$> loadTexture "stonework-015_d100.png" SRGB
                     <*> loadTexture "stonework-015_b020-p050.png" Linear

    ceiling <- Material <$> loadTexture "CrustyConcrete-ColorMap.jpg" SRGB <*> loadTexture "CrustyConcrete-NormalMap.jpg" Linear

    floor <- Material <$> loadTexture "tiles.png" SRGB
                     <*> loadTexture "tiles-normals.png" Linear

    test <- Material <$> loadTexture "test-texture.jpg" SRGB <*> loadTexture "debug-normals.png" Linear
    flat <- Material <$> loadTexture "white.jpg" SRGB <*> loadTexture "flat.jpg" Linear
    gamma <- Material <$> loadTexture "gamma.png" SRGB <*> loadTexture "flat.jpg" Linear
    GL.get GL.errors >>= mapM_ print

    -- test <- return flat
    -- wall1 <- return test
    -- wall2 <- return test
    -- floor <- return test
    -- ceiling <- return test

    -- wall1 <- return test
    -- wall2 <- return test
    -- floor <- return test
    --test <- return gamma
    --wall1 <- return test
    --floor <- return test
    --ceiling <- return test
    -- wall2 <- return test


    sectorLargeRoom <-
      buildSector Blueprint { blueprintVertices = IM.fromList $ zip [0..] $ V.toList room
                            , blueprintWalls = [(0, 1), (13, 14), (14, 27), (27, 0)]
                            , blueprintFloor = (-2)
                            , blueprintCeiling = 20
                            , blueprintFloorMaterial = floor
                            , blueprintCeilingMaterial = ceiling
                            , blueprintWallMaterial = wall1
                            }

    [sectorCol1, sectorCol2, sectorCol3, sectorCol4] <- forM [col1, col2, col3, col4] $ \col ->
      buildSector Blueprint { blueprintVertices = IM.fromList $ zip [0..] $ V.toList col
                            , blueprintWalls = [(0, 1), (1, 2), (2, 3), (3, 0)]
                            , blueprintFloor = (-2)
                            , blueprintCeiling = 20
                            , blueprintFloorMaterial = floor
                            , blueprintCeilingMaterial = ceiling
                            , blueprintWallMaterial = wall1
                            }

    let sectors = [sectorLargeRoom, sectorCol1, sectorCol2, sectorCol3, sectorCol4]

    putStrLn "Loading main shader"
    shaderProg <- createShaderProgram "shaders/vertex/projection-model.glsl"
                                      "shaders/fragment/solid-white.glsl"
    GL.get GL.errors >>= mapM_ print

    spotlightIndex <- withCString "spotlight" $
      GL.glGetSubroutineIndex (unsafeCoerce shaderProg) GL.gl_FRAGMENT_SHADER

    pointIndex <- withCString "omni" $
      GL.glGetSubroutineIndex (unsafeCoerce shaderProg) GL.gl_FRAGMENT_SHADER

    putStrLn "Loading shadow map shader"
    shadowShader <- createShaderProgram "shaders/vertex/shadow.glsl" "shaders/fragment/depth.glsl"
    GL.get GL.errors >>= mapM_ print

    let perspective =
          let fov = 75
              s = recip (tan $ fov * 0.5 * pi / 180)
              far = 100
              near = 1
          in [s ,0 ,0 ,0
             ,0 ,s ,0 ,0
             ,0 ,0 ,-(far / (far - near)) ,-1
             ,0 ,0 ,-((far * near) / (far - near)) ,1]

    let lPerspective =
          let fov = 130
              s = recip (tan $ fov * 0.5 * pi / 180)
              far = 100
              near = 1
          in [s ,0 ,0 ,0
             ,0 ,s ,0 ,0
             ,0 ,0 ,-(far / (far - near)) ,-1
             ,0 ,0 ,-((far * near) / (far - near)) ,1]

    SV.unsafeWith perspective $ \ptr -> do
      putStrLn "Setting perspective matrix"
      GL.UniformLocation loc1 <- GL.get (GL.uniformLocation shaderProg "projection")
      GL.currentProgram $= Just shaderProg
      GL.glUniformMatrix4fv loc1 1 0 ptr
      GL.get GL.errors >>= mapM_ print

    SV.unsafeWith lPerspective $ \ptr -> do
      putStrLn "Setting light perspective matrix (main shader)"
      GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "lightProjection")
      GL.currentProgram $= Just shaderProg
      GL.glUniformMatrix4fv loc 1 0 ptr
      GL.get GL.errors >>= mapM_ print

      putStrLn "Setting light perspective matrix (shadow map shader)"
      GL.UniformLocation loc1 <- GL.get (GL.uniformLocation shadowShader "depthP")
      GL.currentProgram $= Just shadowShader
      GL.glUniformMatrix4fv loc1 1 0 ptr
      GL.get GL.errors >>= mapM_ print

    let bias = [ 0.5, 0, 0, 0
               , 0, 0.5, 0, 0
               , 0, 0, 0.5, 0
               , 0.5, 0.5, 0.5, 1
               ]
    SV.unsafeWith bias $ \ptr -> do
      putStrLn "Setting bias matrix"
      GL.UniformLocation loc1 <- GL.get (GL.uniformLocation shaderProg "bias")
      GL.currentProgram $= Just shaderProg
      GL.glUniformMatrix4fv loc1 1 0 ptr
      GL.get GL.errors >>= mapM_ print

    putStrLn "Getting uniform locations"
    do GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "tex")
       GL.glUniform1i loc 0

    do GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "depthMap")
       GL.glUniform1i loc 1

    do GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "nmap")
       GL.glUniform1i loc 2

    putStrLn "Setting depth test"
    GL.depthFunc $= Just GL.Lequal

    putStrLn "Configuring light UBO"
    shaderId <- unsafeCoerce shaderProg
    blockIndex <- withCString "Light" $ GL.glGetUniformBlockIndex shaderId

    lightsUBO <- GL.genObjectName
    GL.bindBuffer GL.UniformBuffer $= Just lightsUBO
    GL.bindBufferBase GL.IndexedUniformBuffer blockIndex $= Just lightsUBO
    GL.get GL.errors >>= mapM_ print

    tstart <- getCurrentTime
    putStrLn "Generating light frame buffer"
    lightFBO <- genLightFramebufferObject
    putStrLn "Generating light depth textures"
    lightTextures <- V.replicateM 10 genLightDepthMap

    putStrLn "Looping"

    fix (\again (w, currentTime) -> do
      newTime <- getCurrentTime
      let frameTime = newTime `diffUTCTime` currentTime

      events <- unfoldEvents
      let FRP.Out (Scene viewMat lights) w' = runIdentity $
            FRP.stepWire (realToFrac frameTime) events w

      with (distribute viewMat) $ \ptr -> do
        GL.UniformLocation loc1 <- GL.get (GL.uniformLocation shaderProg "view")
        GL.currentProgram $= Just shaderProg
        GL.glUniformMatrix4fv loc1 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

      GL.blend $= GL.Disabled
      GL.depthMask $= GL.Enabled
      GL.depthFunc $= Just GL.Lequal
      GL.colorMask $= GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled

      GL.currentProgram $= Just shadowShader
      GL.bindFramebuffer GL.Framebuffer $= lightFBO
      GL.viewport $= (GL.Position 0 0, GL.Size shadowMapResolution shadowMapResolution)
      GL.cullFace $= Nothing
      lights' <- flip V.mapM (V.zip lights lightTextures) $ \(l, t) -> do
        case lightShape l of
          Spotlight dir _ _ rotationMatrix -> do
            GL.framebufferTexture2D GL.Framebuffer GL.DepthAttachment GL.Texture2D t 0
            GL.clear [GL.DepthBuffer]

            let v = m33_to_m44 rotationMatrix !*! mkTransformation 0 (negate (lightPos l))
            with (distribute v) $ \ptr -> do
              GL.UniformLocation loc <- GL.get (GL.uniformLocation shadowShader "depthV")
              GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

            forM_ sectors $ \s ->
              case s of Sector{..} -> sectorDrawWalls >> sectorDrawFloor

            return (l, t, distribute v)
          Omni ->
            let v = mkTransformation 0 (negate (lightPos l))
            in return (l, t, distribute v)

      GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
      GL.cullFace $= Just GL.Back
      GL.viewport $= (GL.Position 0 0, GL.Size screenWidth screenHeight)
      GL.clear [GL.DepthBuffer]

      GL.currentProgram $= Just shaderProg
      mapM_ drawSectorTextured sectors

      GL.blend $= GL.Enabled
      GL.blendFunc $= (GL.One, GL.One)
      GL.colorMask $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled
      GL.clear [GL.ColorBuffer]
      GL.depthFunc $= Just GL.Equal
      GL.depthMask $= GL.Disabled
      GL.currentProgram $= Just shaderProg
      flip V.mapM_ lights' $ \(l, t, v) -> do
        with v $ \ptr -> do
          GL.currentProgram $= Just shaderProg
          GL.UniformLocation loc <- GL.get (GL.uniformLocation shaderProg "camV")
          GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))

        let i = case lightShape l of
                  Omni -> pointIndex
                  Spotlight{} -> spotlightIndex

        with i $ GL.glUniformSubroutinesuiv GL.gl_FRAGMENT_SHADER 1
        GL.get GL.errors >>= mapM_ print

        GL.activeTexture $= GL.TextureUnit 1
        GL.textureBinding GL.Texture2D $= Just t

        with l $ \ptr -> do
          GL.bindBuffer GL.UniformBuffer $= Just lightsUBO
          GL.bufferData GL.UniformBuffer $= (fromIntegral (sizeOf (undefined :: Light)), ptr, GL.StreamDraw)

        GL.get GL.errors >>= mapM_ print

        -- GL.polygonMode $= (GL.Line, GL.Line)
        mapM_ drawSectorTextured sectors
        GL.get GL.errors >>= mapM_ print

      SDL.glSwapWindow win
      again (w', newTime)) (scene, tstart)

unfoldEvents :: IO [SDL.Event]
unfoldEvents =
  alloca $
  \evtPtr ->
    do r <- SDL.pollEvent evtPtr
       case r of
         0 -> return []
         _ -> (:) <$> peek evtPtr <*> unfoldEvents
