module System.LXD.LXDFile.Utils.String where

import Data.List (intercalate)
import Data.List.Split (splitOn)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old
