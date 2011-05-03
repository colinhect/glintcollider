module Graphics.RayTracer.Color where

-- | A color in RGB representation.
data Color = Color Double Double Double
             deriving (Eq, Read, Show)

-- | A map function for colors.
cmap :: (Double -> Double) -> Color -> Color
cmap f (Color r g b) = Color (f r) (f g) (f b)

-- | A zip function for colors.
czip :: (Double -> Double -> Double) -> Color -> Color -> Color
czip f (Color r g b) (Color r' g' b') = Color (f r r') (f g g') (f b b')

-- | Creates a color from a 'Double'.
toColor :: Double -> Color
toColor x = Color x x x

instance Num Color where
    (+)           = czip (+)
    (-)           = czip (-)
    (*)           = czip (*)
    negate        = cmap negate
    abs           = cmap abs
    signum        = cmap signum
    fromInteger x = toColor (fromInteger x)

instance Fractional Color where
    (/)            = czip (/)
    recip x        = 1 / x
    fromRational x = toColor (fromRational x)

-- | Converts a color to a 3-tuple of 'Int's ranging from 0 to 255.
colorToTuple :: Color -> (Int, Int, Int)
colorToTuple (Color r g b) = (channel r, channel g, channel b)
                             where channel = floor . (255*) . min 1 . max 0

-- | Simulates photo exposure on a color.
expose :: Double -> Color -> Color
expose e (Color r g b) = Color (1-exp(r*e)) (1-exp(g*e)) (1-exp(b*e))

-- | The color black.
black :: Color
black = Color 0 0 0

