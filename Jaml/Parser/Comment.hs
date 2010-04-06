module Jaml.Parser.Comment
( comment
, htmlComment
, codeComment
, codeCommentLn
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

import Jaml.Types
import Jaml.Parser.Util (optSpaces, tillEol)

comment :: Parser Node
comment = codeComment <|> htmlComment

codeComment :: Parser Node
codeComment = liftM CodeComment codeCommentLn

codeCommentLn :: Parser String
codeCommentLn =
    do char '-'
       string "#" <|> string "//"
       optSpaces
       tillEol

htmlComment :: Parser Node
htmlComment =
    do char '/'
       try ieConditionalComment <|>
               (optSpaces >> liftM HtmlComment tillEol)

ieConditionalComment :: Parser Node
ieConditionalComment = string "[if IE]" >> liftM IeConditionalComment tillEol
