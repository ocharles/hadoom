{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Sector where

import Control.Applicative
import Control.Category
import Control.Lens hiding (indices)
import Data.Foldable (any, foldMap)
import Data.Function (on)
import Data.Int (Int32)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Foreign (Storable(..), castPtr, nullPtr, plusPtr)
import Foreign.C (CFloat)
import Geometry
import Graphics.GL
import Linear as L hiding (outer)
import Material
import Prelude hiding (any, floor, ceiling, (.), id)
import Shader
import Util
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

data Vertex =
  Vertex {vPos :: {-# UNPACK #-} !(V3 CFloat)
         ,vNorm :: {-# UNPACK #-} !(V3 CFloat)
         ,vTangent :: {-# UNPACK #-} !(V3 CFloat)
         ,vBitangent :: {-# UNPACK #-} !(V3 CFloat)
         ,vUV :: {-# UNPACK #-} !(V2 CFloat)}
  deriving (Show)

instance Storable Vertex where
  sizeOf ~(Vertex p n t bn uv) = sizeOf p + sizeOf n + sizeOf t + sizeOf bn +
                                 sizeOf uv
  alignment _ = 0
  peek ptr =
    Vertex <$>
    peek (castPtr ptr) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined) `plusPtr`
                   sizeOf (vTangent undefined))) <*>
    peek (castPtr (ptr `plusPtr`
                   sizeOf (vPos undefined) `plusPtr`
                   sizeOf (vNorm undefined) `plusPtr`
                   sizeOf (vTangent undefined) `plusPtr`
                   sizeOf (vBitangent undefined)))
  poke ptr (Vertex p n t bn uv) =
    do poke (castPtr ptr) p
       poke (castPtr (ptr `plusPtr` sizeOf p)) n
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n)) t
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n `plusPtr`
                      sizeOf t))
            bn
       poke (castPtr (ptr `plusPtr` sizeOf p `plusPtr` sizeOf n `plusPtr`
                      sizeOf t `plusPtr` sizeOf bn))
            uv

data Blueprint =
  Blueprint {blueprintVertices :: IM.IntMap (V2 CFloat)
            ,blueprintWalls :: V.Vector (Int,Int)
            ,blueprintFloor :: CFloat
            ,blueprintCeiling :: CFloat
            ,blueprintFloorMaterial :: Material
            ,blueprintCeilingMaterial :: Material
            ,blueprintWallMaterial :: Material}

data Sector =
  Sector {sectorDrawWalls :: IO ()
         ,sectorDrawFloor :: IO ()
         ,sectorDrawCeiling :: IO ()
         ,sectorFloorMaterial :: Material
         ,sectorCeilingMaterial :: Material
         ,sectorWallMaterial :: Material}

rayLineIntersection :: (Epsilon a,Fractional a,Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Maybe (V2 a)
rayLineIntersection p r q q' =
  let s = q' - q
      cross2 (V2 a b) (V2 x y) = a * y - b * x
      pToQ = q - p
      tNum = pToQ `cross2` s
      uNum = pToQ `cross2` r
  in case r `cross2` s of
       denom
         | nearZero denom -> Nothing
         | otherwise ->
           let u = uNum / denom
               t = tNum / denom
           in if 0 <= u && u <= 1
                 then Just (p + r ^* t)
                 else Nothing

makeSimple :: (Epsilon a,Fractional a,Ord a) => V.Vector (V2 a) -> V.Vector (V2 a) -> V.Vector (V2 a)
makeSimple inner outer =
  let xMost = comparing (view _x)
      m = V.maximumBy xMost inner
      mIndex = V.maxIndexBy xMost inner
      indexedOuter = V.imap (,) outer
      edges =
        V.zip (V.imap (,) outer)
              (V.tail indexedOuter <> indexedOuter)
      intersections =
        V.map (\(s@(_,start_),e@(_,end_)) ->
                 ((rayLineIntersection m
                                       (V2 1 0)
                                       start_
                                       end_)
                 ,s
                 ,e))
              edges
      (Just i,start,end) =
        V.minimumBy
          (\(x,_,_) (y,_,_) ->
             case (x,y) of
               (Nothing,Nothing) -> EQ
               (Just _,Nothing) -> LT
               (Nothing,Just _) -> GT
               (Just a,Just b) ->
                 comparing (qd m) a b)
          intersections
      (pIndex,p) =
        V.maximumBy (xMost `on` snd)
                    [start,end]
      containing =
        V.filter (\((_,a),(j,b),(_,c)) ->
                    j /= pIndex &&
                    triangleArea a b c <
                    0 &&
                    pointInTriangle m i p b)
                 ((V.zip3 indexedOuter
                          (V.drop 1 (indexedOuter <> indexedOuter))
                          (V.drop 2 (indexedOuter <> indexedOuter))))
      angleAgainst x =
        dot (V2 1 0) .
        subtract x
      (_,(minimalReflex,_),_) =
        V.minimumBy
          (\(_,(_,a),_) (_,(_,b),_) ->
             foldMap (\f -> f a b)
                     (comparing (angleAgainst m) :
                      comparing (qd m) :
                      []))
          containing
      splitOuter
      -- | nearZero (i - start) = error "makeSimple: startIndex"
      -- | nearZero (i - end) = error "makeSimple: endIndex"
        | V.null containing = pIndex
        | otherwise = minimalReflex
  in case V.splitAt splitOuter outer of
       (before,after) ->
         before <>
         V.take 1 after <>
         V.take (succ (V.length inner))
                (V.drop mIndex inner <>
                 inner) <>
         after

triangulate :: (Epsilon a,Fractional a,Ord a) => V.Vector (V2 a) -> V.Vector Int
triangulate = collapseAndTriangulate
  where collapseAndTriangulate vs =
          collapse vs (go (addIndices vs))

takeFirst :: (a -> Bool) -> V.Vector a -> V.Vector a
takeFirst f =
  V.take 1 .
  V.filter f

isEar :: (Epsilon a,Ord a,Fractional a) => ((t,V2 a),(t1,V2 a),(t2,V2 a),V.Vector (t3,V2 a)) -> Bool
isEar ((_,a),(_,b),(_,c),otherVertices) =
  let area = triangleArea a b c
      containsOther =
        any (pointInTriangle a b c .
             snd)
            (V.filter (\(_,v) ->
                         not (nearZero (a - v)) &&
                         not (nearZero (b - v)) &&
                         not (nearZero (c - v)))
                      otherVertices)
  in area > 0 && not containsOther

go :: (Epsilon b,Ord b,Fractional b) => V.Vector (a,V2 b) -> V.Vector a
go s
  | V.length s < 3 = empty
  | otherwise =
    do (v0@(n0,_),(n1,_),v2@(n2,_),others) <- takeFirst isEar (separate s)
       [n0,n2,n1] <>
         go (v0 `V.cons`
             (v2 `V.cons` others))

go1 :: (Ord a1,Epsilon a1,Fractional a1) => V.Vector (a,V2 a1) -> (V.Vector a,V.Vector (a,V2 a1))
go1 s
  | V.length s < 3 = error "Done"
  | otherwise =
    let (v0@(n0,_),(n1,_),v2@(n2,_),others) =
          V.head (takeFirst isEar (separate s))
    in ([n2,n1,n0]
       ,(v0 `V.cons`
         (v2 `V.cons` others)))

addIndices :: V.Vector a -> V.Vector (Int,a)
addIndices vertices = V.imap (,) vertices

separate :: V.Vector a -> V.Vector (a,a,a,V.Vector a)
separate vertices =
  let n = V.length vertices
      doubleVerts = vertices <> vertices
  in V.zip4 vertices
            (V.drop 1 doubleVerts)
            (V.drop 2 doubleVerts)
            (V.imap (\i _ ->
                       V.take (n - 3)
                              (V.drop (i + 3) doubleVerts))
                    vertices)

collapse :: Epsilon a => V.Vector a -> V.Vector Int -> V.Vector Int
collapse vs =
  V.map (\i ->
           let v = vs V.! i
           in fst (V.head (V.filter (nearZero . (v -) . snd)
                                    (V.imap (,) vs))))

buildSector :: Blueprint -> IO Sector
buildSector Blueprint{..} =
  do vao <- initializeVAO
     initializeVBO
     configureVertexAttributes
     initializeIBO
     return (Sector {sectorDrawWalls =
                       do glBindVertexArray vao
                          glDrawElements GL_TRIANGLES
                                         (fromIntegral (V.length wallIndices))
                                         GL_UNSIGNED_INT
                                         nullPtr
                    ,sectorDrawFloor =
                       do glBindVertexArray vao
                          glDrawElements
                            GL_TRIANGLES
                            (fromIntegral (V.length floorIndices))
                            GL_UNSIGNED_INT
                            (nullPtr `plusPtr`
                             fromIntegral
                               (sizeOf (0 :: Int32) *
                                V.length wallIndices))
                    ,sectorDrawCeiling =
                       do glBindVertexArray vao
                          glDrawElements
                            GL_TRIANGLES
                            (fromIntegral (V.length ceilingIndices))
                            GL_UNSIGNED_INT
                            (nullPtr `plusPtr`
                             fromIntegral
                               (sizeOf (0 :: Int32) *
                                (V.length wallIndices + V.length floorIndices)))
                    ,sectorWallMaterial = blueprintWallMaterial
                    ,sectorFloorMaterial = blueprintFloorMaterial
                    ,sectorCeilingMaterial = blueprintCeilingMaterial})
  where initializeVAO =
          do vao <- overPtr (glGenVertexArrays 1)
             glBindVertexArray vao
             return vao
        initializeVBO =
          do vbo <- overPtr (glGenBuffers 1)
             glBindBuffer GL_ARRAY_BUFFER vbo
             let vertices = wallVertices <> floorVertices <> ceilingVertices
             SV.unsafeWith
               (V.convert vertices)
               (\verticesPtr ->
                  glBufferData
                    GL_ARRAY_BUFFER
                    (fromIntegral
                       (V.length vertices *
                        sizeOf (undefined :: Vertex)))
                    (castPtr verticesPtr)
                    GL_STATIC_DRAW)
        configureVertexAttributes =
          do let stride =
                   fromIntegral (sizeOf (undefined :: Vertex))
                 normalOffset =
                   fromIntegral (sizeOf (0 :: V3 CFloat))
                 tangentOffset =
                   normalOffset +
                   fromIntegral (sizeOf (0 :: V3 CFloat))
                 bitangentOffset =
                   tangentOffset +
                   fromIntegral (sizeOf (0 :: V3 CFloat))
                 uvOffset =
                   bitangentOffset +
                   fromIntegral (sizeOf (0 :: V3 CFloat))
             glVertexAttribPointer positionAttribute 3 GL_FLOAT GL_FALSE stride nullPtr
             glEnableVertexAttribArray positionAttribute
             glVertexAttribPointer normalAttribute
                                   3
                                   GL_FLOAT
                                   GL_FALSE
                                   stride
                                   (nullPtr `plusPtr` normalOffset)
             glEnableVertexAttribArray normalAttribute
             glVertexAttribPointer tangentAttribute
                                   3
                                   GL_FLOAT
                                   GL_FALSE
                                   stride
                                   (nullPtr `plusPtr` tangentOffset)
             glEnableVertexAttribArray tangentAttribute
             glVertexAttribPointer bitangentAttribute
                                   3
                                   GL_FLOAT
                                   GL_FALSE
                                   stride
                                   (nullPtr `plusPtr` bitangentOffset)
             glEnableVertexAttribArray bitangentAttribute
             glVertexAttribPointer uvAttribute
                                   2
                                   GL_FLOAT
                                   GL_FALSE
                                   stride
                                   (nullPtr `plusPtr` uvOffset)
             glEnableVertexAttribArray uvAttribute
        wallVertices =
          V.concatMap
            (\(s,e) ->
               expandEdge (blueprintVertices IM.! s)
                          (blueprintVertices IM.! e))
            blueprintWalls
          where expandEdge start@(V2 x1 y1) end@(V2 x2 y2) =
                  let wallV = end ^-^ start
                      wallLen = norm wallV
                      n =
                        normalize (case perp wallV of
                                     V2 x y ->
                                       V3 x 0 y)
                      u = wallLen / textureSize
                      v =
                        (blueprintCeiling - blueprintFloor) /
                        textureSize
                  in V.fromList
                       (getZipList
                          (Vertex <$>
                           ZipList [V3 x1 blueprintFloor y1
                                   ,V3 x1 blueprintCeiling y1
                                   ,V3 x2 blueprintFloor y2
                                   ,V3 x2 blueprintCeiling y2] <*>
                           ZipList (repeat n) <*>
                           ZipList (repeat (normalize (case end - start of
                                                         V2 x y ->
                                                           V3 x 0 y))) <*>
                           ZipList (repeat (V3 0 1 0)) <*>
                           ZipList [V2 0 v,V2 0 0,V2 u v,V2 u 0]))
        wallIndices =
          V.concatMap
            id
            (V.imap (\m _ ->
                       let n = m * 4
                       in V.map fromIntegral [n,n + 2,n + 1,n + 1,n + 2,n + 3])
                    blueprintWalls)
        floorVertices =
          V.map (\(V2 x y) ->
                   Vertex (V3 x blueprintFloor y)
                          (V3 0 1 0)
                          (V3 1 0 0)
                          (V3 0 0 (-1))
                          (V2 x y ^*
                           recip textureSize))
                (V.fromList (IM.elems blueprintVertices))
        ceilingVertices =
          V.map (\(Vertex p n t bn uv) ->
                   Vertex (p & _y .~ blueprintCeiling)
                          (negate n)
                          t
                          bn
                          uv)
                floorVertices
        floorIndices =
          let n =
                fromIntegral (V.length wallVertices)
          in fmap (fromIntegral . (+ n))
                  (triangulate (V.fromList (IM.elems blueprintVertices)))
        ceilingIndices =
          let reverseTriangles v =
                case V.splitAt 3 v of
                  (h,t)
                    | V.length h == 3 ->
                      [h V.! 0,h V.! 2,h V.! 1] V.++
                      reverseTriangles t
                  _ -> []
          in V.map (+ (fromIntegral (V.length floorVertices)))
                   (reverseTriangles floorIndices)
        initializeIBO =
          do let indices :: V.Vector Int32
                 indices = wallIndices <> floorIndices <> ceilingIndices
             ibo <- overPtr (glGenBuffers 1)
             glBindBuffer GL_ELEMENT_ARRAY_BUFFER ibo
             SV.unsafeWith
               (V.convert indices)
               (\indicesPtr ->
                  glBufferData
                    GL_ELEMENT_ARRAY_BUFFER
                    (fromIntegral
                       (V.length indices *
                        sizeOf (0 :: Int32)))
                    (castPtr indicesPtr)
                    GL_STATIC_DRAW)

textureSize = 2.5

drawSectorTextured :: Sector -> IO ()
drawSectorTextured Sector{..} =
  do activateMaterial sectorWallMaterial
     sectorDrawWalls
     activateMaterial sectorFloorMaterial
     sectorDrawFloor
     activateMaterial sectorCeilingMaterial
     sectorDrawCeiling

drawSectorGeometry :: Sector -> IO ()
drawSectorGeometry Sector{..} =
  do sectorDrawWalls
     sectorDrawFloor
     sectorDrawCeiling

drawSectorWalls :: Sector -> IO ()
drawSectorWalls Sector{..} = sectorDrawWalls
