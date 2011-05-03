module Graphics.RayTracer.Material where

import Graphics.RayTracer.Color

-- | Describes how a surface interacts with light rays. Contains the color, specular term,
-- shininess (specular power), reflectivity, and opacity.
data Material =
        Material {
            matColor     :: Color,
            matSpecular  :: Color,
            matShininess :: Int,
            matRefl      :: Double,
            matOpacity   :: Double
        } deriving (Eq, Read, Show)

