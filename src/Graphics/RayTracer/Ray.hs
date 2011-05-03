module Graphics.RayTracer.Ray where

import Graphics.RayTracer.Vector

-- | A line segment in 3-space with an origin and a direction (must be of unit length).
data Ray =
        Ray {
            rayOrigin :: Vector,
            rayDir    :: Vector
        } deriving (Eq, Show)

