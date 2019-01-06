module AOC201504
  ( partOne
  , partTwo
  )
where

import           Data.Hash.MD5                  ( Str(..)
                                                , md5s
                                                )
import           Data.List                      ( find
                                                , isPrefixOf
                                                )

mine :: (String -> Bool) -> String -> Maybe (Int, String)
mine p key = find (p . snd) searchSpace
  where searchSpace = map (\x -> (x, md5s $ Str (key ++ show x))) [1 ..]

partOne :: String -> Maybe (Int, String)
partOne = mine ("00000" `isPrefixOf`)

partTwo :: String -> Maybe (Int, String)
partTwo = mine ("000000" `isPrefixOf`)
