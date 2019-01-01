module Main where

import qualified AOC201501                      ( partOne
                                                , partTwo
                                                )

main :: IO ()
main = do
  input <- getContents
  let solution = AOC201501.partTwo input
  putStrLn $ "Solution: " ++ show solution
