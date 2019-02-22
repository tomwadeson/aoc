module Main where

import qualified AOC201505                      ( partOne
                                                , partTwo
                                                )

main :: IO ()
main = do
  input <- getContents
  let solution = AOC201505.partTwo (lines input)
  putStrLn $ "Solution: " ++ show solution
