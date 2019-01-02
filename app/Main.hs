module Main where

import qualified AOC201502                      ( partOne
                                                , partTwo
                                                )

main :: IO ()
main = do
  input <- getContents
  let solution = AOC201502.partTwo input
  putStrLn $ "Solution: " ++ show solution
