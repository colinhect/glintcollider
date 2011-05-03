module Graphics.RayTracer.Scene where

import Graphics.RayTracer.Primitive
import Graphics.RayTracer.Light

-- | A scene contains a list of primitives and lights, along with the options for
-- how to render the scene.
data Scene = Scene SceneOptions [Primitive] [Light] deriving (Eq, Read, Show)

type ImageSize = (Int, Int)

-- | Describes specific options on how the scene is rendered.
data SceneOptions =
        SceneOptions {
            optImageSize   :: ImageSize,
            optFocalLength :: Double,
            optLenseHeight :: Double,
            optExposure    :: Double
        } deriving (Eq, Read, Show)

