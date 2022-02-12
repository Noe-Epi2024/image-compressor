module Tab
    ( getPixel,
      parseSwoosh,
      tabCoordinates,
      tabColors,
      fillTab,
      Pixel(..),
      Color(..),
      Position(..),
    ) where

import Control.Exception
import System.Exit
import Text.Read
import Text.Printf
import Data.List
import Data.String
import System.Random
import Numeric
import Error

type Position = (Int, Int)
type Color = (Float, Float, Float)
type Pixel = (Position, Color)

-- Parsing

getPixel :: String -> Pixel
getPixel str = (position, color)
    where
        x = (words (map parseSwoosh coordinateValue)) !! 0
        y = (words (map parseSwoosh coordinateValue)) !! 1
        coordinateValue = ((words str) !! 0) \\"()"
        r = (words (map parseSwoosh colorValue)) !! 0
        g = (words (map parseSwoosh colorValue)) !! 1
        b = (words (map parseSwoosh colorValue)) !! 2
        colorValue =  ((words str) !! 1) \\ "()"
        position = tabCoordinates (x, y)
        color =  tabColors (r, g, b) 

parseSwoosh :: Char -> Char
parseSwoosh ',' = ' '
parseSwoosh x = x

tabCoordinates :: (String, String) -> (Int, Int)
tabCoordinates coordinate = ((read (fst coordinate) :: Int), (read (snd coordinate) :: Int))

tabColors :: (String, String, String) -> Color
tabColors (r, g, b) = ((read (r) :: Float), (read (g) :: Float), (read (b) :: Float))

fillTab :: String -> [Pixel]
fillTab x = map getPixel (lines x)