module AOC201503
  ( partOne
  , partTwo
  )
where

import           Control.Monad.State.Lazy       ( State
                                                , modify
                                                , execState
                                                )
import           Data.Set                       ( Set
                                                , insert
                                                , size
                                                , union
                                                , singleton
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( partition )

data Direction = North | East | South | West
  deriving (Eq, Show)

parseDirections :: String -> [Direction]
parseDirections = mapMaybe parseDirection
 where
  parseDirection '^' = Just North
  parseDirection '>' = Just East
  parseDirection 'v' = Just South
  parseDirection '<' = Just West
  parseDirection _   = Nothing

data Location = Location Integer Integer
  deriving (Eq, Ord, Show)

data DeliveryProgress = DeliveryProgress {
  deliverer :: Location,
  visited :: Set Location
}

partOne :: String -> Int
partOne input = size (deliver directions)
  where directions = parseDirections input

partTwo :: String -> Int
partTwo input = size (santaDeliveries `union` robotDeliveries)
 where
  (santaDirections, robotDirections) = alternatively (parseDirections input)
  santaDeliveries                    = deliver santaDirections
  robotDeliveries                    = deliver robotDirections

deliver :: [Direction] -> Set Location
deliver = visited . (`execState` initial) . traverse_
  (modify . followDirection)
  where initial = DeliveryProgress (Location 0 0) (singleton $ Location 0 0)

followDirection :: Direction -> DeliveryProgress -> DeliveryProgress
followDirection direction (DeliveryProgress deliverer visited) =
  DeliveryProgress deliverer' visited'
 where
  deliverer' = move deliverer direction
  visited'   = insert deliverer' visited
  move (Location x y) North = Location x (y + 1)
  move (Location x y) East  = Location (x + 1) y
  move (Location x y) South = Location x (y - 1)
  move (Location x y) West  = Location (x - 1) y

alternatively :: [a] -> ([a], [a])
alternatively as =
  let (lefts, rights) = partition ((== 1) . snd) $ zip as (cycle [1, 2])
  in  (map fst lefts, map fst rights)
