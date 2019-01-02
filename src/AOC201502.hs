module AOC201502
  ( partOne
  , partTwo
  )
where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Void                      ( Void )
import           Data.Maybe                     ( fromMaybe )

data Box = Box {
  length :: Int,
  width  :: Int,
  height :: Int
} deriving (Show, Eq)

type Parser = Parsec Void String

parseBox :: Parser Box
parseBox = do
  l <- L.decimal
  single 'x'
  w <- L.decimal
  single 'x'
  h <- L.decimal
  pure (Box l w h)

parseBoxes :: Parser [Box]
parseBoxes = many (parseBox <* sc) where sc = L.space space1 empty empty

partOne :: String -> Int
partOne input = sum . map paperForBox $ boxes
  where boxes = fromMaybe [] (parseMaybe parseBoxes input)

surfaceArea :: Box -> Int
surfaceArea (Box l w h) = 2 * l * w + 2 * w * h + 2 * h * l

areaOfSmallestSide :: Box -> Int
areaOfSmallestSide (Box l w h) = minimum [l * w, w * h, h * l]

paperForBox :: Box -> Int
paperForBox b = surfaceArea b + areaOfSmallestSide b

partTwo :: String -> Int
partTwo input = sum . map ribbonForBox $ boxes
  where boxes = fromMaybe [] (parseMaybe parseBoxes input)

ribbonForBox :: Box -> Int
ribbonForBox b = shortestPerimeter b + volume b

shortestPerimeter :: Box -> Int
shortestPerimeter (Box l w h) = minimum [l + w, w + h, h + l] * 2

volume :: Box -> Int
volume (Box l w h) = l * w * h
