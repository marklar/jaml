module Jaml.Parser.Attribute
( cssSelector
, classSelector
, idSelector
, attrs
, attrsOfType
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

import Jaml.Types
import Jaml.Parser.Util (identifier, optSpaces, inQuotes)
import Jaml.Parser.JavaScript.Value (code)

-- HAML selectors --
cssSelector :: Parser Attr
cssSelector = idSelector <|> classSelector

idSelector :: Parser Attr
idSelector = selector "id" '#'

classSelector :: Parser Attr
classSelector = selector "class" '.'

selector :: String -> Char -> Parser Attr
selector name prefix =
    char prefix >> liftM mkLit identifier
    where mkLit s = Attr (name, [Literal s])

attrs :: Parser [Attr]
attrs = try rubyAttrs <|> jsAttrs <|> htmlAttrs

-- JavaScript-style attributes --
jsAttrs :: Parser [Attr]
jsAttrs = attrsOfType $ attrPair identifier (string ":")

-- Ruby-style Attributes --
rubyAttrs :: Parser [Attr]
rubyAttrs =
    attrsOfType $ attrPair rubySymbol (string "=>")
    where rubySymbol = char ':' >> identifier

-- HTML-style Attributes --
-- http://haml-lang.com/docs/yardoc/HAML_REFERENCE.md.html#htmlstyle_attributes_
htmlAttrs :: Parser [Attr]
htmlAttrs =
    between (char '(') (char ')') (sepBy htmlAttrPair spaces)

-- HTML-style boolean attributes can be written just like HTML
--    %input(selected)  OR   %input(selected=true)
htmlAttrPair :: Parser Attr
htmlAttrPair =
    do optSpaces  -- fixme: can include newline
       key <- identifier  -- FIXME: can also contain colon!!!
       liftM (\v -> Attr (key, [Literal v])) (htmlPairValue <|> return key)

htmlPairValue :: Parser String
htmlPairValue =
    do char '='
       inQuotes <|> many (noneOf " )")

attrPair :: Parser String -> Parser String -> Parser Attr
attrPair key sep =
    do optSpaces
       k <- key <|> inQuotes
       optSpaces
       sep <?> "hash separator"
       optSpaces
       v <- code <?> "JavaScript value"
       optSpaces
       return $ Attr (k, [JS v])

-- attrsOfType :: Parser Attr -> Parser [Attr]
attrsOfType pair =
    between (char '{') (char '}') (sepBy pair sep)
    where sep = char ',' >> spaces
