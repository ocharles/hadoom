{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

-- | The abstract description of a world, as a typed DSL.
module Hadoom.World
       (SceneElementType(..), SectorProperties(..), WorldExpr(..),
        PWorld(..), letrec)
       where

import BasePrelude
import Data.TList
import Linear
import System.IO

-- | The set of types of scene elements in a world.
data SceneElementType = TVertex | TWall | TSector | TMaterial | TTexture | TWorld

-- | The description of a sector.
data SectorProperties =
  SectorProperties {sectorFloor :: Float -- ^ The level of the floor.
                   ,sectorCeiling :: Float -- ^ The level of the ceiling.
                   }

-- | The syntax to describe a world
data WorldExpr :: (SceneElementType -> *) -> SceneElementType -> * where
  -- | Letrec, allowing the binding of expressions to reusable names.
  Let
    :: (TList f ts -> TList (WorldExpr f) ts) -- let ...
    -> (TList f ts -> WorldExpr f t)          -- in ...
    -> WorldExpr f t

  -- | The ability to use a name.
  Var :: f t -> WorldExpr f t

  -- | Vertices in the 2D plane of the  world.
  Vertex :: V2 Float -> WorldExpr f TVertex

  -- | Walls between vertices, with the front sector (the wall "faces" this
  -- sector), and optional back sector. 'Nothing' implies the wall has its back
  -- to the void.
  Wall
    :: WorldExpr f TVertex -> WorldExpr f TVertex         -- Start and end vertices
    -> WorldExpr f TSector -> Maybe (WorldExpr f TSector) -- Front and (optional) back sector
    -> Maybe (WorldExpr f TMaterial) -> Maybe (WorldExpr f TMaterial) -> Maybe (WorldExpr f TMaterial)
    -> WorldExpr f TWall

  -- | Sectors take some basic properties, a list of vertices, and their material
  -- attributes.
  Sector
    :: SectorProperties
    -> [WorldExpr f TVertex] -- vertices
    -> WorldExpr f TMaterial -- floor
    -> WorldExpr f TMaterial -- ceiling
    -> WorldExpr f TSector

  -- | A material is the composition of a basic diffuse texture along with an
  -- optional normal map.
  Material
    :: WorldExpr f TTexture
    -> Maybe (WorldExpr f TTexture)
    -> WorldExpr f TMaterial

  -- | A texture refers to an image on disk.
  Texture :: FilePath -> WorldExpr f TTexture

  World
    :: [WorldExpr f TSector]
    -> [WorldExpr f TWall]
    -> WorldExpr f TWorld

newtype PWorld t = PWorld (forall f. WorldExpr f t)

letrec :: (TList (WorldExpr f) ts -> TList (WorldExpr f) ts)
       -> (TList (WorldExpr f) ts -> WorldExpr f t)
       -> WorldExpr f t
letrec es e =
  Let (\xs -> es (tmap Var xs))
      (\xs -> e (tmap Var xs))
