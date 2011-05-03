module Graphics.RayTracer.Primitive where

import Graphics.RayTracer.Material
import Graphics.RayTracer.Vector

-- | A primitive geometrical shape.  All primitives have a 'Material'.
data Primitive =
        -- | Creates a sphere primitive.
        Sphere {
            sphMaterial :: Material,
            sphOrigin   :: Vector,
            sphRadius   :: Double
        }
        |
        -- | Creates a plane primitive.
        Plane {
            plnMaterial :: Material,
            plnNormal   :: Vector,
            plnDist     :: Double
        } deriving (Eq, Read, Show)

