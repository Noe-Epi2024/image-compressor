module Main where

import System.Environment
import Data.List
import Data.Char
import Data.Typeable
import Control.Exception
import System.Exit
import Text.Read
import Numeric

import Error
import Tab
import Cluster

main :: IO ()
main = do
    args <- getArgs
    if (length (args) == 0)
        then errorExit
    else
        case getOpts defaultConf args of
            Nothing -> errorExit
            Just conf -> imageCompressor (pathValue conf) (clustersValue conf) (convergenceValue conf)

data Conf = Conf { color :: Int
                 , convergence :: Float
                 , path :: String
                 } deriving (Show, Read)

defaultConf :: Conf
defaultConf = Conf 0 0 ""

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("-n":value:xs) = case readMaybe value :: Maybe Int of 
    Just b -> getOpts (conf {color = b}) xs
    Nothing -> Nothing
getOpts conf ("-l":value:xs) = case readMaybe value :: Maybe Float of 
    Just b -> getOpts (conf {convergence = b}) xs
    Nothing -> Nothing
getOpts conf ("-f":value:xs) = getOpts (conf {path = value}) xs
getOpts conf _ = Nothing

clustersValue :: Conf -> Int
clustersValue (Conf colors _ _)
    | colors > 0 = colors
    | otherwise = 0

checkCluster :: Int -> IO Int
checkCluster x
    | x == 0 = errorExiti
    | otherwise = return x

convergenceValue :: Conf -> Float
convergenceValue (Conf _ convergence _)
    | convergence > 0 = convergence
    | otherwise = 0

checkConvergence :: Float -> IO Float
checkConvergence x
    | x == 0 = errorExitf
    | otherwise = return x

pathValue :: Conf -> FilePath
pathValue (Conf _ _ path) = path

imageCompressor :: FilePath -> Int -> Float -> IO ()
imageCompressor path cluster conv = do
    file <- catch (readFile path) handle
    n <- checkCluster cluster
    clust <- initClusters n
    l <- checkConvergence conv
    displayCluster (clust, []) (fillTab file) l
    where
        handle :: SomeException -> IO String
        handle error = putStrLn "Error file does not exist." >> errorExits