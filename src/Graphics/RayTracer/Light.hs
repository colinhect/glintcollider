module Graphics.RayTracer.Light where

import Graphics.RayTracer.Color
import Graphics.RayTracer.Vector

-- | A light.
data Light =
        -- | Constructs a point light given the origin, color, and radius.
        PointLight {
            plOrigin :: Vector,
            plColor  :: Color,
            plRadius :: Double
        } deriving (Eq, Read, Show)

