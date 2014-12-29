{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module TestWorld where

import BasePrelude
import Data.TList
import Linear
import Material
import Hadoom.World

testWorld :: PWorld TWorld
testWorld =
  PWorld (letrec (\_ ->
                    Texture "flat.jpg" Linear :::
                    TNil)
                 (\(flat ::: _) ->
                    letrec (\_ ->
                              Hadoom.World.Material (Texture "DHTP/textures/gstone2.png" SRGB)
                                                    (Just flat) :::
                              Hadoom.World.Material (Texture "DHTP/flats/flat5.png" SRGB)
                                                    (Just flat) :::
                              Hadoom.World.Material (Texture "DHTP/flats/ceil3_3.png" SRGB)
                                                    (Just flat) :::
                              Hadoom.World.Material (Texture "DHTP/textures/bigdoor2.png" SRGB)
                                                    (Just flat) :::
                              Vertex (V2 (-2)
                                         (-2)) :::
                              Vertex (V2 (-1.2)
                                         (-2)) :::
                              Vertex (V2 1.2 (-2)) :::
                              Vertex (V2 2 (-2)) :::
                              Vertex (V2 2 2) :::
                              Vertex (V2 (-2) 2) :::
                              Vertex (V2 1.2 (-40)) :::
                              Vertex (V2 (-1.2)
                                         (-40)) :::
                              TNil)
                           (\(wt ::: ft ::: ct ::: lt ::: v1 ::: v2 ::: v3 ::: v4 ::: v5 ::: v6 ::: v7 ::: v8 ::: _) ->
                              letrec (\_ ->
                                        Sector (SectorProperties 0 3)
                                               [v1,v2,v3,v4,v5,v6]
                                               ft
                                               ct :::
                                        Sector (SectorProperties 0 2.5)
                                               [v2,v8,v7,v3]
                                               ft
                                               ct :::
                                        TNil)
                                     (\(s1 ::: s2 ::: TNil) ->
                                        World [s1,s2]
                                              [Wall v1 v2 s1 Nothing (Just wt) Nothing Nothing
                                              ,Wall v2
                                                    v3
                                                    s1
                                                    (Just s2)
                                                    (Just lt)
                                                    (Just wt)
                                                    (Just wt)
                                              ,Wall v3 v4 s1 Nothing (Just wt) Nothing Nothing
                                              ,Wall v4 v5 s1 Nothing (Just wt) Nothing Nothing
                                              ,Wall v5 v6 s1 Nothing (Just wt) Nothing Nothing
                                              ,Wall v6 v1 s1 Nothing (Just wt) Nothing Nothing
                                              ,Wall v2 v8 s2 Nothing (Just wt) Nothing Nothing
                                              ,Wall v8 v7 s2 Nothing (Just wt) Nothing Nothing
                                              ,Wall v7 v3 s2 Nothing (Just wt) Nothing Nothing]))))
