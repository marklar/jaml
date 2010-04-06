module Jaml.Parser.JavaScript.BacktickedValue
( backtickedCode
, backtickedSingleVal
, backtickedArraySansQuotes
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM, liftM2)

import Jaml.Types
import Jaml.Util (joinStrs)
import Jaml.Parser.Util (optSpaces, allBetween, stringInside)

{- For backwards compatibility. -}


backtickedCode :: Parser String
backtickedCode = allBetween "`" "`"

backtickedSingleVal :: Parser String
backtickedSingleVal =
    between (tick  >> quote) (quote >> tick)
                (many $ noneOf "'")
    where tick = char '`'
          quote = many (char '\'')  -- really, just 1.

-- Result lacks brackets.
backtickedArraySansQuotes :: Parser String
backtickedArraySansQuotes =
    liftM (joinStrs ", ") $
          between (string "`[") (string "]`")
                      (sepBy arrayVal (char ','))

arrayVal :: Parser String
arrayVal =
    between optSpaces (many $ noneOf ",]")
                (stringInside '\'' <|> stringInside '"' <|> many (noneOf " ,]"))

