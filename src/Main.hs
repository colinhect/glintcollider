module Main where

import Codec.Image.PPM
import Graphics.RayTracer

import System.Environment (getArgs)
import System.CPUTime

main :: IO ()
main = do
       args <- getArgs
       case args of
           [input, output] -> do
                              putStrLn ("Tracing scene '"++input++"'...")
                              sceneData <- readFile input
                              let scene = ((read sceneData) :: Scene) 
                              startTime <- getCPUTime
                              writeFile output $ ppm (map (map colorToTuple) (traceScene scene))
                              endTime <- getCPUTime
                              let elapsedTime = fromIntegral (endTime-startTime) * (0.000000000001 :: Double)
                              putStrLn ("Done in "++show elapsedTime++" seconds")
           ["--help"]      -> putStrLn help
           _               -> putStrLn invalidArgs 

help :: String
help = "Usage: glintcollider [options] scene image\nTraces the scene file and saves it to the image file."

invalidArgs :: String
invalidArgs = "glintcollider: invalid arguments\nUsage: For basic information, try the '--help' option."

