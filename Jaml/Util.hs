module Jaml.Util
( joinStrs
) where

import Data.List (intersperse)

joinStrs :: String -> [String] -> String
joinStrs sep strs = concat $ intersperse sep strs
