module Geometry where

import Prelude

import Linear

triangleArea :: Fractional a => V2 a -> V2 a -> V2 a -> a
triangleArea a b c =
  let toV3 (V2 x y) = V3 x y 1
      det =
        det33 (V3 (toV3 a)
                  (toV3 b)
                  (toV3 c))
  in 0.5 * det

pointInTriangle :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
pointInTriangle p0@(V2 p0x p0y) p1@(V2 p1x p1y) p2@(V2 p2x p2y) (V2 px py) =
  let area = triangleArea p0 p1 p2
      s = 1 / (2 * area) * (p0y * p2x - p0x * p2y + (p2y - p0y) * px + (p0x - p2x) * py)
      t = 1 / (2 * area) * (p0x * p1y - p0y * p1x + (p0y - p1y) * px + (p1x - p0x) * py)
  in s > 0 && t > 0 && (1 - s - t) > 0
