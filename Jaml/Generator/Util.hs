module Jaml.Generator.Util
( bite
, escSingles
) where

{- "Bite" == eat newline.
   By default, jaml.ns() prepends a newline.
   To prevent that, call _J.x() before _j.ns().
-}
bite = "_J.x();"

escSingles :: String -> String
escSingles s = concat $ map esc s
    where esc '\'' = "\\'"
          esc c    = [c]
