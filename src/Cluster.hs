module Cluster
    ( initClusters,
      loop,
      displayCluster,
      findMin,
    ) where

import Text.Printf
import System.Random ( newStdGen, Random(randomRs, randomRIO), RandomGen (next) )
import Error
import Tab

type Cluster = (Color, [Pixel])

initCluster :: IO Cluster
initCluster = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)
    return ((r,g,b), [])

initClusters :: Int -> IO [Cluster]
initClusters 0 = return []
initClusters n = do
    c <- initCluster
    cs <- initClusters (n - 1)
    return (c:cs)

addColor :: Color -> Color -> Color
addColor (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

getColorMean' :: [Pixel] -> Color
getColorMean' [] = (0, 0, 0)
getColorMean' ((_, c1):xs) = addColor c1 (getColorMean' xs)

divColorByInt :: Color -> Float -> Color
divColorByInt (r, g, b) n = (r / n, g / n, b / n)

getColorMean :: [Pixel] -> Color
getColorMean pix = divColorByInt (getColorMean' pix) (fromIntegral len :: Float)
    where len = length pix

updateCluster :: Cluster -> Cluster
updateCluster (color, []) = (color, [])
updateCluster (color, pixels) = (getColorMean pixels, [])

euclideanDistance :: Color -> Color -> Float
euclideanDistance x@(r1,g1,b1) y@(r2,g2,b2) = sqrt((r1-r2)*(r1-r2)+(g1-g2)*(g1-g2)+(b1-b2)*(b1-b2))

findMin :: Color -> [Cluster] -> Cluster
findMin color [x] = x
findMin color (x@(col1,_):xs@(col2,_):xy)
    | euclideanDistance color col1 < euclideanDistance color col2 = findMin color (x:xy)
    | otherwise = findMin color (xs:xy)

addPixelCluster :: [Cluster] -> Pixel -> Cluster -> [Cluster]
addPixelCluster [] pixel cluster = []
addPixelCluster (x@(col1,pix):xs) pixel cluster@(col2,_)
    | col1 == col2 = (col1,pixel:pix):xs
    | otherwise = x:addPixelCluster xs pixel cluster

recalculateCluster :: [Cluster] -> [Pixel] -> [Cluster]
recalculateCluster newclusters [] = newclusters
recalculateCluster newclusters (x@(_,col1):xs) = recalculateCluster (addPixelCluster newclusters x (findMin col1 newclusters)) xs

convergence :: [Cluster] -> [Cluster] -> Float -> Bool
convergence [] [] _ = True
convergence [] _ _ = False
convergence _ [] _ = False
convergence ((col1, _) : xs1) ((col2, _) : xs2) e
    | euclideanDistance col1 col2 > e = False
    | otherwise = convergence xs1 xs2 e

loop :: ([Cluster], [Cluster]) -> [Pixel] -> Float -> [Cluster]
loop (now, next) pixel e =
    if convergence now next e
        then loop (next, recalculateCluster (map updateCluster next) pixel) pixel e
        else next

displayCluster :: ([Cluster], [Cluster]) -> [Pixel] -> Float -> IO ()
displayCluster (clusters, x) pixel e = printCluster (loop (clusters, recalculateCluster clusters pixel) pixel e)

printCluster :: [Cluster] -> IO ()
printCluster [] = return ()
printCluster ((col,pix):xs) = do
    putStrLn "--"
    printColor col
    putStrLn "-"
    printPixel pix
    printCluster xs

printColor :: Color -> IO ()
printColor (r,g,b) = printf "(%.0f,%.0f,%.0f)\n" r g b

printPixel :: [Pixel] -> IO ()
printPixel [] = return ()
printPixel (((x,y),(r,g,b)):xs) = do
    printf "(%d,%d) (%.0f,%.0f,%.0f)\n" x y r g b
    printPixel xs