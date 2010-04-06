module Jaml.Generator.Attribute
( attrToJS
, attrValStr
) where

import Data.List (partition)

import Jaml.Util (joinStrs)
import Jaml.Types

attrToJS :: String -> Attr -> String

-- If Attr has no values, don't output it.
attrToJS _ (Attr (_, [])) = ""

{- An HTML attribute may have >1 value.  Space-separated.

   When creating strings within JS: wrap in SINGLE quotes.
   That way, resultant HTML can easily contain double quotes.
-}
attrToJS idPrefix (Attr (name, vals)) =
    "_j.s(\' " ++ name ++ "=\"" ++ idPrefix' ++ valsStr ++ "\"');"
    where
          -- Only for "id" attr, prepend user-supplied prefix (if any).
          idPrefix' = case name of "id" -> idPrefix
                                   _ -> ""

          -- Compute value String.  Combine non-blank literals & JS.
          valsStr = joinStrs " " $ filter (not.null) [literalsStr, scriptsStr]

          -- JS.  Always wrap values in parens, for safe interpreting.
          -- (The parens aren't always necessary, but I didn't feel like being
          -- clever to determine when.  [Adds code complexity for nil pay-off.])
          scriptsStr = if null scripts
                       then ""
                       else concat [ "'+("
                                   , joinStrs ")+' '+(" $ map attrValStr scripts
                                   , ")+'"
                                   ]

          -- String literals (i.e. direct pass-throughs from template).
          literalsStr = joinStrs " " $ map attrValStr literals

          -- Treat literals and JS snippets differently...
          (literals, scripts) = partition isLiteral vals
          isLiteral (Literal _) = True
          isLiteral _ = False

-- Unwrap string.
attrValStr :: AttrVal -> String
attrValStr (Literal s) = s
attrValStr (JS s) = s
