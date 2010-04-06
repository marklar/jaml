module Jaml.Parser.QuotedString
( quotedString
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

import Jaml.Parser.Util (stringInside)

quotedString :: Parser String
quotedString = quotedWith '\'' <|> quotedWith '"'

-- FixMe: doesn't account for antiquotes: e.g. "'fool\\\'s gold'"
quotedWith :: Char -> Parser String
quotedWith c = liftM (wrap [c]) $ stringInside c

wrap :: String -> String -> String
wrap o i = o++i++o
