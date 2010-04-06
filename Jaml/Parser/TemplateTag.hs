module Jaml.Parser.TemplateTag
( templateTag
) where

import Text.ParserCombinators.Parsec

import Jaml.Types
import Jaml.Parser.Attribute (idSelector, classSelector, attrsOfType)
import Jaml.Parser.Util (tillEol, allBetween, trimmed)
import Jaml.Parser.JavaScript.BacktickedValue (backtickedArraySansQuotes, backtickedSingleVal)

-- %template.Glyde.LiveSearchDomain#_render{ :args =>
--    `['is_first' /*bool*/, 'width' /*int*/, 'height' /*int*/]`
--        OR
--    [is_first /*bool*/, width /*int*/, height /*int*/]
-- }
templateTag :: Parser Node
templateTag =
    do string "%template"
       cs <- many1 classSelector <?> "JavaScript class name"
       m  <- idSelector          <?> "JavaScript method name"
       as <- attrsOfType templateAttrPair <|> return []
       tillEol     -- skips '{', if provided
       return (TemplateTag cs m as)

-- FixMe.  This type signature doesn't work.
-- templateAttrPair :: Parser (String,String)
templateAttrPair =
    do trimmed argsKey
       separator
       val <- trimmed argsVal
       return ("args", val)

-- This is kinda complicated, in order to be backwards-compatible.
argsVal :: Parser String
argsVal = try backtickedArraySansQuotes
          <|> try backtickedSingleVal
          <|> allBetween "[" "]"
          <?> "JavaScript array"

argsKey :: Parser String
argsKey = many (char ':') >> string "args"

separator :: Parser String
separator = string ":" <|> string "=>" <?> "hash k-v separator"
