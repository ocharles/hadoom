{-# LANGUAGE DataKinds #-}
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

data State =
  State {sVertices :: IntMap (Point V2 Double)
        ,sSectors :: [Polygon IntMap.Key]}
  deriving (Show)

data SectorBuilder =
  SectorBuilder {sbState :: Either Operation State}
  deriving (Show)

data Operation = AddSector AddSectorState
  deriving (Show)

data AddSectorState =
  AddSectorState {asVertices :: NonEmpty IntMap.Key
                 ,asState :: State}
  deriving (Show)

emptySectorBuilder :: SectorBuilder
emptySectorBuilder = SectorBuilder (Right (State mempty mempty))

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

data Snapshot = Snapshot
  deriving (Eq, Show)

addVertex :: Point V2 Double -> SectorBuilder -> SectorBuilder -> (Maybe Snapshot, SectorBuilder)
addVertex coords snapshot =
  either expandCurrentPolygon beginNewPolygon .
  sbState
  where beginNewPolygon oldState =
          let (sVertices',vId) =
                insertMax (sVertices oldState) coords
          in (Just Snapshot
             ,SectorBuilder {sbState =
                               Left (AddSector (AddSectorState
                                                  (pure vId)
                                                  (splitExistingLines
                                                     (oldState {sVertices = sVertices'})
                                                     (vId,coords))))})
        expandCurrentPolygon (AddSector (AddSectorState (initialPoint :| ps) s))
          | not (nearZero (coords .-.
                           (sVertices s IntMap.! initialPoint))) =
            let (sVertices',vId) =
                  insertMax (sVertices s) coords
            in (Nothing
               ,SectorBuilder {sbState =
                                 Left (AddSector (AddSectorState
                                                    (initialPoint :| vId : ps)
                                                    (splitExistingLines
                                                       (s {sVertices = sVertices'})
                                                       (vId,coords))))})
          | otherwise =
            case reverse ps of
              (p2:p3:ps') ->
                (Nothing
                ,SectorBuilder {sbState =
                                  Right (s {sSectors =
                                              sSectors s ++
                                              [Polygon initialPoint p2 p3 ps']})})
              _ -> (Nothing,snapshot)

insertMax :: IntMap a -> a -> (IntMap a, IntMap.Key)
insertMax im a
  | IntMap.null im = (IntMap.singleton 0 a,0)
  | otherwise =
    let k = succ (fst (IntMap.findMax im))
    in (IntMap.insert k a im,k)

splitExistingLines :: State -> (IntMap.Key, Point V2 Double) -> State
splitExistingLines (State vertices sectors) (pId, p) =
  (State vertices (map splitPolygon sectors))
  where splitPolygon s =
          let splitLine ls
                | pointOnLine (fmap (vertices IntMap.!) ls) p = start ls :| [pId]
                | otherwise = start ls :| []
          in joinPolygon (splitLine <$> linesOfPolygon s)

-- TODO Rewrite without relying on partiality (pattern match)
joinPolygon :: Polygon (NonEmpty a) -> Polygon a
joinPolygon (Polygon a b c d) =
  let v1:v2:v3:vs = toList a <> toList b <> toList c <> concatMap toList d
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
