module Main where

import qualified AOC201506                      ( partOne
                                                , partTwo
                                                )

main :: IO ()
main = do
  input <- getContents
  let solution = AOC201506.partOne input
  putStrLn $ "Solution: " ++ show solution
