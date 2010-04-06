module Jaml.Parser.Tag
( tag
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)
import Data.List (partition)

import Jaml.Types
import Jaml.Parser.Util (optSpaces, identifier)
import Jaml.Parser.Attribute (cssSelector, attrs)

tag :: Parser Node
tag =
    do n  <- try name <|> return "div"
       ss <- many cssSelector <|> return []
       as <- attrs <|> return []
       gs <- gators
       c  <- if isSelfClosing n then return SelfClosing else content
       optSpaces
       return (Tag n (unifyClassSelectors $ ss ++ as) gs c)

unifyClassSelectors :: [Attr] -> [Attr]
unifyClassSelectors selectors = (combine classes) : others
    where (classes, others) = partition isClass selectors
          combine cs = Attr ("class", concat $ map vals cs)
          vals (Attr (_, vs)) = vs
          isClass (Attr ("class", _)) = True
          isClass _ = False

isSelfClosing :: String -> Bool
isSelfClosing n = n `elem` selfClosingTagNames
selfClosingTagNames :: [String] = ["meta", "img", "link", "script", "br", "hr"]

-- First InGator, then OutGator.  (Or NoGator in either spot.)
gators :: Parser (Gator,Gator)
gators =
    do a <- outGator <|> inGator <|> return NoGator
       b <- case a of
              NoGator  -> return NoGator
              OutGator -> inGator  <|> return NoGator
              InGator  -> outGator <|> return NoGator
       return $ case a of InGator -> (b,a)
                          _ -> (a,b)
    where outGator = char '>' >> return OutGator
          inGator  = char '<' >> return InGator

content :: Parser TagContent
content =
    selfClose <|> try noisyCode <|> try plainText <|> return EmptyContent

selfClose :: Parser TagContent
selfClose = 
    do char '/'
       -- there shouldn't be anything else here until Eol
       return SelfClosing

noisyCode :: Parser TagContent
noisyCode =
    do optSpaces >> char '=' >> optSpaces
       liftM NoisyCode beforeEol

plainText :: Parser TagContent
plainText =
    do optSpaces
       liftM content beforeEol
    where content "" = EmptyContent
          content s  = PlainText s

name :: Parser String
name = char '%' >> identifier

beforeEol :: Parser String
beforeEol = many1 (noneOf "\n")