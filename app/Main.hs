module Main where

import qualified AOC201503                      ( partOne
                                                , partTwo
                                                )

main :: IO ()
main = do
  input <- getContents
  let solution = AOC201503.partTwo input
  putStrLn $ "Solution: " ++ show solution
