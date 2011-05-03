module Graphics.RayTracer.Vector where

-- | A 3-dimensional vector.
data Vector = Vector Double Double Double 
              deriving (Eq, Read, Show)

-- | A map functions for vectors.
vmap :: (Double -> Double) -> Vector -> Vector
vmap f (Vector x y z) = Vector (f x) (f y) (f z)

-- | A zip functions for vectors.
vzip :: (Double -> Double -> Double) -> Vector -> Vector -> Vector
vzip f (Vector x y z) (Vector x' y' z') = Vector (f x x') (f y y') (f z z')

-- | Creates a vector from a 'Double'.
toVector :: Double -> Vector
toVector x = Vector x x x

instance Num Vector where
    (+)           = vzip (+)
    (-)           = vzip (-)
    (*)           = vzip (*)
    negate        = vmap negate
    abs           = vmap abs
    signum        = vmap signum
    fromInteger x = toVector (fromInteger x)

instance Fractional Vector where
    (/)            = vzip (/)
    recip x        = 1 / x
    fromRational x = toVector (fromRational x)

-- | Dot product of two vectors.
dot :: Vector -> Vector -> Double
dot (Vector x y z) (Vector x' y' z') = x*x' + y*y' + z*z'

-- | Cross product of two vectors.
cross :: Vector -> Vector -> Vector
cross (Vector x y z) (Vector x' y' z') = Vector (y*z'-z*y') (z*x'-x*z') (x*y'-y*x')

-- | Magnitude of vector.
magnitude :: Vector -> Double
magnitude v = sqrt (dot v v)

-- | Normalize vector.
normalize :: Vector -> Vector
normalize v = v / toVector (magnitude v)

