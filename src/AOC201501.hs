module AOC201501
  ( partOne
  , partTwo
  )
where

import           Data.List                      ( find )
import           Data.Maybe                     ( mapMaybe )
import           Prelude                 hiding ( floor )

partOne :: String -> Int
partOne = sum . map directionToInt . parseDirections

data Direction = Up | Down deriving Show

parseDirections :: String -> [Direction]
parseDirections = mapMaybe parseDirection
 where
  parseDirection '(' = Just Up
  parseDirection ')' = Just Down
  parseDirection _   = Nothing

directionToInt :: Direction -> Int
directionToInt Up   = 1
directionToInt Down = -1

partTwo :: String -> Maybe InstructionCount
partTwo = instructionCountAtFloor (Floor (-1)) . parseDirections

instructionCountAtFloor :: Floor -> [Direction] -> Maybe InstructionCount
instructionCountAtFloor f =
  fmap instruction
    . find (\b -> floor b == f)
    . scanl
        (\(Building (Floor f) (InstructionCount i)) direction -> Building
          (Floor (f + directionToInt direction))
          (InstructionCount (i + 1))
        )
        groundFloor
  where groundFloor = Building (Floor 0) (InstructionCount 0)

newtype Floor = Floor Int deriving (Show, Eq)

newtype InstructionCount = InstructionCount Int deriving (Show, Eq)

data Building =
   Building { floor :: Floor, instruction :: InstructionCount } deriving Show
