module Jaml.Parser.JavaScript.Value
( code
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM, liftM2)

import Jaml.Types
import Jaml.Parser.QuotedString (quotedString)
import Jaml.Parser.Util (allBounded)
import Jaml.Parser.JavaScript.BacktickedValue (backtickedCode)

{- Used only as VALUE in Attribute Pair.
   ',' separates k:v pairs.
   '}' terminates a Ruby Hash or JS object.
-}
code :: Parser String
code = backtickedCode <|> jsUpToAnyOf ",}"

{-
   Our goal is not actually to *parse* the JS.
   Our goal is merely to gather it up: to find where it ends.

   We know our JS will end with one of a set of particular chars,
   but we don't know whether there will be any instances of
   those chars "escaped" somewhere in the middle.
   For example, we may want to find a JS array and know that
   the array will end with ']', but we don't know whether
   there might be other arrays nested in that array,
   or whether there's a string which happens to contain ']',
   and so on.  We wouldn't want to mistake one of those ']'s
   as the end of our array.

   So here's our plan...

   We first gather as much "simple" code as we can find,
   until we either:
     - reach one of our JS-final chars, or
     - encounter the beginning of "complex" code.
   "Complex" code is something like the nested array or
   ']'-containing string from the examples above -- which might
   contain one of our JS-final chars where that char doesn't
   signify the end.
   
   If we encounter "complex" code, then we use the same
   technique to find the end of that "complex" code, recursing
   as necessary.

   We're done when we've consumed all input and have nothing
   left but one of our JS-final chars.
-}
-- (Cannot use sepBy: elides the separators.)
jsUpToAnyOf :: [Char] -> Parser String
jsUpToAnyOf chars =
    do str <- liftM2 (++) (simpleUpTo chars) complex
       case str of "" -> return ""
                   _ -> liftM ((++) str) (jsUpToAnyOf chars)

{-
  "Simple": contains only simple values, without:
    - strings
    - objects, or
    - arrays
-}
-- ToDo: pass in a parser instead of characters.
simpleUpTo :: [Char] -> Parser String
simpleUpTo chars =
    do spaces
       many $ noneOf $ concat ["\n", chars, complexPrefixChars]
       -- ToDo: remove any spaces at end!
       -- import Text.Regex.Posix
    where complexPrefixChars = "{\"'[(/"

{-
  "Complex": parses one 'complex' piece of JS:
    - object
    - array
    - argList (i.e. array without brackets)
    - string
    - comment
    - regexp
-}
complex :: Parser String
complex =
    choice [ object, array, argList  -- mutually nestable
           , quotedString
           , cStyleComment, regexp  -- these 2: order matters!
           , return ""
           ]

-- make inner funs of above?
array   :: Parser String = jsBetween '[' ']'
object  :: Parser String = jsBetween '{' '}'
argList :: Parser String = jsBetween '(' ')'

cStyleComment :: Parser String = allBounded "/*" "*/"
regexp        :: Parser String = allBounded "/" "/"

-- Uses jsUpToAnyOf to recurse through ...
jsBetween :: Char -> Char -> Parser String
jsBetween l r =
    liftM (\js -> concat [[l], js, [r]]) $
          between (char l) (char r) (jsUpToAnyOf [r])
