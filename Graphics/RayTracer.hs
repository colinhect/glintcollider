module Graphics.RayTracer (
    Color(..),
    Scene(..),
    traceScene,
    colorToTuple 
) where

import Graphics.RayTracer.Color
import Graphics.RayTracer.Intercept
import Graphics.RayTracer.Light
import Graphics.RayTracer.Material
import Graphics.RayTracer.Primitive
import Graphics.RayTracer.Scene
import Graphics.RayTracer.Ray
import Graphics.RayTracer.Vector

import Data.List (sort)
import Data.List.Split (splitEvery)

-- | Epsilon constant.
eps :: Double
eps = 0.00001

-- | Returns the list of intercepts of a ray intersected against a list of primitives.
intersectRay :: Ray -> [Primitive] -> [Intercept]
intersectRay _ [] = []

-- Ray-sphere intersection
intersectRay (Ray ro rd) ((Sphere sm so sr):ps) 
    | dsc > 0 && dst > eps = Intercept dst io n vr refl refr sm : intersectRay vr ps
    | otherwise            = intersectRay vr ps
                             where 
                                   -- Discriminant
                                   dsc  = b * b - c
                                   a    = ro - so
                                   b    = dot a rd
                                   c    = dot a a - sr*sr
                                               
                                   -- Resulting intersection
                                   dst  = (-b) - sqrt dsc
                                   io   = ro + rd * (toVector dst)
                                   n    = normalize (io - so)
                                   vr   = Ray ro rd
                                   refl = Ray io (normalize (rd - toVector (2 * dot rd n) * n))
                                   refr = Ray ro rd --

-- Ray-plane intersection
intersectRay (Ray ro rd) ((Plane pm pn pd):ps)  
    | dsc < 0 && dst > eps = Intercept dst io n vr refl refr pm : intersectRay vr ps
    | otherwise            = intersectRay vr ps
                             where 
                                   -- Discriminant
                                   dsc  = dot pn rd
                                                                    
                                   -- Resulting intersection
                                   dst  = (dot ro pn + pd) / dot rd pn
                                   io   = ro + rd * (toVector dst)
                                   n    = pn
                                   vr   = Ray ro rd
                                   refl = Ray io (normalize (rd - toVector (2 * dot rd n) * n))
                                   refr = Ray ro rd --

-- | Returns the resulting pixel value of the given ray traced in the scene.
traceRay :: Ray -> Scene -> Int -> Color
traceRay r (Scene os ps ls) d 
    | null is || d < 1 = black
    | otherwise        = computeLighting i ps ls + refl * traceRay (interReflRay i) (Scene os ps ls) (d-1)
                         where is   = sort (intersectRay r ps)
                               i    = head is
                               refl = toColor (matRefl (interMat i))

-- | Returns the resulting color of the given intercept given the lights.
computeLighting :: Intercept -> [Primitive] -> [Light] -> Color
computeLighting _ _ [] = black
computeLighting (Intercept dst o n vr refl refr m) ps ((PointLight lo lc lr):ls) 
    | null is   = lambert i l + phong i l + computeLighting i ps ls
    | otherwise = black + computeLighting i ps ls
                  where is = intersectRay (Ray o (normalize (lo-o))) ps
                        i  = Intercept dst o n vr refl refr m
                        l  = (PointLight lo lc lr)

-- | Computes the lambert lighting term.
lambert :: Intercept -> Light -> Color
lambert (Intercept dst o n _ _ _ m) (PointLight lo lc lr) 
    | dst > lr  = black
    | otherwise = lc * matColor m * toColor term * toColor (1 - dst / lr)
                  where term = dot n (normalize (lo-o))

-- | Computes the phong lighting term.
phong :: Intercept -> Light -> Color
phong (Intercept dst o n vr _ _ m) (PointLight lo lc lr) 
    | dst > lr  = black
    | otherwise = lc * matColor m * matSpecular m * toColor term * toColor (1 - dst / lr)
                  where term  = term' ^ matShininess m
                        term' = max (dot (normalize (normalize (lo-o) - rayDir vr)) n) 0

-- | Generates a list of all conically projected rays pointing in the positive Z direction.
conicRays :: ImageSize -> Double -> Double -> [Ray]
conicRays (w, h) fl lh = [Ray ro (normalize ((Vector x y 0)-ro)) | y <- bet (fromIntegral h) lh, x <- bet (fromIntegral w) (lh*aspect)] 
                         where bet n s = map ((*) (s*2/n)) [-(n/2)+0.5..(n/2)-0.5]
                               ro      = Vector 0 0 (-fl)
                               aspect  = fromIntegral w / fromIntegral h

-- | Traces the given scene, returning the image in the form of a list of list of colors.
traceScene :: Scene -> [[Color]]
traceScene (Scene os ps ls) = map (map (expose e)) (reverse (splitEvery w [(traceRay r (Scene os ps ls) 5) | r <- conicRays (w, h) fl lh]))
                              where (w, h) = optImageSize os
                                    fl     = optFocalLength os
                                    lh     = optLenseHeight os
                                    e      = optExposure os
