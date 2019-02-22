module AOC201505 where

import           Data.List                      ( group
                                                , isInfixOf
                                                )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set

partOne :: [String] -> Int
partOne strings =
    let
        niceStrings = filter
            (\s ->
                atLeastThreeVowels s
                    && atLeastOneRepitition s
                    && noBannedSubstrings s
            )
            strings
    in  length niceStrings

atLeastThreeVowels :: String -> Bool
atLeastThreeVowels = (>= 3) . length . filter isVowel
    where isVowel c = c `elem` "aeiou"

atLeastOneRepitition :: String -> Bool
atLeastOneRepitition = any ((> 1) . length) . group

noBannedSubstrings :: String -> Bool
noBannedSubstrings str = no "ab" && no "cd" && no "pq" && no "xy"
    where no = not . (`isInfixOf` str)

partTwo :: [String] -> Int
partTwo strings =
    let niceStrings = filter
            (\s -> surrounds s && (not . Set.null $ nonOverlappingPairs s))
            strings
    in  length niceStrings

nonOverlappingPairs :: (Ord a) => [a] -> Set.Set [a]
nonOverlappingPairs =
    Map.keysSet . Map.filter (> 1) . count . removeSingleConsecutive . sliding 2

surrounds :: (Eq a) => [a] -> Bool
surrounds = any (\[x, y, z] -> x == z) . sliding 3

sliding :: Int -> [a] -> [[a]]
sliding n xs | n > 0 && n <= length xs = take n xs : sliding n (drop 1 xs)
             | otherwise               = []

removeSingleConsecutive :: (Eq a) => [a] -> [a]
removeSingleConsecutive xs =
    let grouped = map (\x -> (x, length x)) $ group xs
        dropped = map (\(x, l) -> if l > 1 then drop 1 x else x) grouped
    in  concat dropped

count :: (Ord a) => [a] -> Map.Map a Int
count = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty
