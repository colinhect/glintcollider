module Graphics.RayTracer.Intercept where

import Graphics.RayTracer.Material
import Graphics.RayTracer.Ray
import Graphics.RayTracer.Vector

-- | A point of intersection between a 'Ray' and a 'Primitive'.  Contains the distance
-- from the view ray origin, the origin of the intersection, the normal on the primitive
-- the originating view ray, the reflected/refracted ray, and the 'Material' of the 
-- primitive that was intersected.
data Intercept =
        Intercept {
            interDist    :: Double,
            interOrigin  :: Vector,
            interNormal  :: Vector,
            interViewRay :: Ray,
            interReflRay :: Ray,
            interRefrRay :: Ray,
            interMat     :: Material
        } deriving (Eq, Show)

instance Ord Intercept where
    compare (Intercept d _ _ _ _ _ _) (Intercept d' _ _ _ _ _ _) | d == d'   = EQ
                                                                 | d <= d'   = LT
                                                                 | otherwise = GT

