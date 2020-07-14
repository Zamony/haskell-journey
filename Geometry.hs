module Geometry (
    sphereVolume,
    sphereArea,
) where

sphereVolume :: Double->Double
sphereVolume r = (4.0 / 3.0) * pi * r * r * r

sphereArea :: Double->Double
sphereArea r = 4.0 * pi * r * r