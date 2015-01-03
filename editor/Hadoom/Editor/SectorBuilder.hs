{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Hadoom.Editor.SectorBuilder where

import BasePrelude hiding (union)
import Data.List.NonEmpty (NonEmpty(..))
import Linear
import Linear.Affine
import Reactive.Banana

import Data.TList
import Hadoom.World
import Material (ColorSpace(..))

-- | The 'SectorBuilder' is responsible for building a new sector out of a
-- series of mouse clicks, corresponding to each vertex of the sector. Sector
-- building is complete when the user returns to the first sector.
data SectorBuilder =
  SectorBuilder {sbInProgress :: Maybe (NonEmpty (Point V2 Double))
                -- ^ A sector that is in the process of being built. This
                -- non-empty list corresponds to each of the vertices in the
                -- sector.
                ,sbComplete :: [NonEmpty (Point V2 Double)]
                -- ^ A list of all sectors that have been successfully built.
                }

-- | A 'SectorBuilder' that has not yet built any sectors, and does not
-- have any sectors in the process of being built.
emptySectorBuilder :: SectorBuilder
emptySectorBuilder = SectorBuilder Nothing []

data SectorBuilderEvents t =
  SectorBuilderEvents {evAddVertex :: Event t (Point V2 Double)
                      ,evAbort :: Event t ()}

-- | Build a reactive 'SectorBuilder'. Given an event containing the coordinates
-- (in map-world space), this will progress the 'SectorBuilder', resulting in:
--   * The beginning of a new sector
--   * The progress of an in-progress sector
--   * The completion of a new sector
mkSectorBuilder :: SectorBuilderEvents t
              -> Behavior t SectorBuilder
mkSectorBuilder SectorBuilderEvents{..} =
  accumB emptySectorBuilder unionedEvents
  where unionedEvents =
          (addVertex <$> evAddVertex) `union`
          (abort <$ evAbort)
        abort =
          \sb -> sb {sbInProgress = Nothing}
        addVertex coords =
          \sb ->
            case sbInProgress sb of
              Nothing ->
                sb {sbInProgress =
                      Just (coords :| [])}
              Just (initialPoint :| ps)
                | coords /= initialPoint ->
                  sb {sbInProgress =
                        Just (initialPoint :| coords : ps)}
                | otherwise ->
                  sb {sbInProgress = Nothing
                     ,sbComplete =
                        (initialPoint :| reverse ps) :
                        sbComplete sb}

toWorld :: SectorBuilder -> PWorld TWorld
toWorld sb =
  let sectors :: WorldExpr f TMaterial
              -> [(WorldExpr f TSector,[WorldExpr f TWall])]
      sectors mat =
        map (\(v :| vs) ->
               let vertices = v : vs
                   walls =
                     zip vertices
                         (vs ++
                          [v])
                   s =
                     Sector (SectorProperties 0 3)
                            (map (Vertex . fmap realToFrac) vertices)
                            mat
                            mat
               in (s
                  ,map (\(v1,v2) ->
                          Wall (Vertex (realToFrac <$> v2))
                               (Vertex (realToFrac <$> v1))
                               (WallFace s
                                         (Just mat)
                                         (Just mat)
                                         (Just mat))
                               Nothing)
                       walls))
            (sbComplete sb)
  in PWorld (letrec (\_ ->
                       Material (Texture "test-texture.jpg" SRGB)
                                (Just (Texture "flat.jpg" Linear)) :::
                       TNil)
                    (\(t ::: _) ->
                       World (map fst (sectors t))
                             (concatMap snd (sectors t))))
