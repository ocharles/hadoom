{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Hadoom.Editor.SectorBuilder where
import BasePrelude hiding (lines, union)
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty(..))
import Hadoom.Geometry
import Linear
import Linear.Affine
import Reactive.Banana
import qualified Data.IntMap.Strict as IntMap

type VertexId = IntMap.Key

data SectorBuilder =
  SectorBuilder {sbVertices :: IntMap (Point V2 Double)
                ,sbSectors :: IntMap (Polygon VertexId)
                ,sbState :: State}
  deriving (Show)

data State
  = AddSector (NonEmpty IntMap.Key)
  | SelectSector (Maybe IntMap.Key)
  deriving (Show)

emptySectorBuilder :: SectorBuilder
emptySectorBuilder =
  SectorBuilder mempty
                mempty
                (SelectSector Nothing)

data SectorBuilderEvents t =
  SectorBuilderEvents {evAddVertex :: Event t (Point V2 Double)
                      ,evAbort :: Event t ()}

mkSectorBuilder :: SectorBuilderEvents t -> Behavior t SectorBuilder
mkSectorBuilder SectorBuilderEvents{..} =
  let events =
        unions [(flip addVertex <$> latestSnapshot) <@>
                evAddVertex
               ,(const <$>
                 ((Nothing,) <$>
                  latestSnapshot)) <@
                evAbort]
      (snapshot,sectorBuilder) =
        mapAccum emptySectorBuilder events
      latestSnapshot =
        stepper emptySectorBuilder (sectorBuilder <@ filterJust snapshot)
  in sectorBuilder

data Snapshot =
  Snapshot
  deriving (Eq,Show)

equalPoints :: (Affine p,Epsilon (Diff p a),Num a)
            => p a -> p a -> Bool
equalPoints a b = nearZero (a .-. b)

addVertex :: Point V2 Double
          -> SectorBuilder
          -> SectorBuilder
          -> (Maybe Snapshot,SectorBuilder)
addVertex coords snapshot sb =
  case sbState sb of
    AddSector (v1 :| vs)
      | equalPoints coords
                    (sbVertices sb IntMap.! v1) ->
        case reverse vs of
          (v2:v3:vs') ->
            (Nothing
            ,sb {sbSectors =
                   fst (insertMax (sbSectors sb)
                                  (Polygon v1 v2 v3 vs'))
                ,sbState = SelectSector Nothing})
          _ -> (Nothing,snapshot)
    _ ->
      let (sVertices',vId) =
            insertMax (sbVertices sb) coords
          sb' =
            splitExistingLines (sb {sbVertices = sVertices'})
                               (vId,coords)
      in case sbState sb of
           AddSector (v1 :| vs) ->
             (Nothing
             ,sb' {sbState =
                     AddSector (v1 :| vId : vs)})
           _ ->
             (Just Snapshot
             ,sb' {sbState = AddSector (pure vId)})

insertMax :: IntMap a -> a -> (IntMap a, IntMap.Key)
insertMax im a
  | IntMap.null im = (IntMap.singleton 0 a,0)
  | otherwise =
    let k = succ (fst (IntMap.findMax im))
    in (IntMap.insert k a im,k)

splitExistingLines :: SectorBuilder -> (IntMap.Key, Point V2 Double) -> SectorBuilder
splitExistingLines sb (pId,p) =
  sb {sbSectors = fmap splitPolygon (sbSectors sb)}
  where splitPolygon s =
          let splitLine ls
                | pointOnLine (fmap (sbVertices sb IntMap.!) ls)
                              p =
                  start ls :|
                  [pId]
                | otherwise = start ls :| []
          in joinPolygon (splitLine <$> linesOfPolygon s)

-- TODO Rewrite without relying on partiality (pattern match)
joinPolygon :: Foldable f => Polygon (f a) -> Polygon a
joinPolygon (Polygon a b c ds) =
  let v1:v2:v3:vs = toList a <> toList b <> toList c <> concatMap toList ds
  in Polygon v1 v2 v3 vs

linesOfPolygon :: Polygon a -> Polygon (LineSegment a)
linesOfPolygon (Polygon v1 v2 v3 []) =
  Polygon (LineSegment v1 v2)
          (LineSegment v2 v3)
          (LineSegment v3 v1)
          []
linesOfPolygon (Polygon v1 v2 v3 vs@(v4:vs')) =
  Polygon (LineSegment v1 v2)
          (LineSegment v2 v3)
          (LineSegment v3 v4)
          (zipWith LineSegment
                   vs
                   (vs' <>
                    [v1]))

closePolygon :: NonEmpty a -> [LineSegment a]
closePolygon (v :| vs) = zipWith LineSegment (v : vs) (vs ++ [v])
