module Jaml.Parser.JavaScript.Line
( silentJS
, noisyJS
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

import Jaml.Types
import Jaml.Parser.Util (tillEol, optSpaces)

silentJS :: Parser Node
silentJS =
    do char '-' >> optSpaces
       liftM SilentJS tillEol  -- don't use 'between' (which consumes)

noisyJS :: Parser Node
noisyJS =
    do char '=' >> optSpaces
       liftM NoisyJS tillEol
