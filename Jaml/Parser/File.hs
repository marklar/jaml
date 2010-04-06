module Jaml.Parser.File
( jamlFile
) where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM, liftM2)

import Jaml.Types
import Jaml.Parser.Comment (htmlComment, codeComment, codeCommentLn)
import Jaml.Parser.TemplateTag (templateTag)
import Jaml.Parser.Tag (tag)
import Jaml.Parser.Util (blankLn, tillEol, optSpaces)
import Jaml.Parser.JavaScript.Line (silentJS, noisyJS)

jamlFile :: Parser [Ast]
jamlFile =
    do ts <- many jamlTemplate
       many blankLn
       optSpaces
       eof
       return ts

jamlTemplate :: Parser Ast
jamlTemplate =
    do many (blankLn <|> codeCommentLn) -- choice
       liftM2 (Ast 0) templateTag (children 1)

children :: Int -> Parser [Ast]
children depth = 
    do many $ try blankLn -- 'try' allows backtracking
       many $ child depth

child :: Int -> Parser Ast
child depth =
    do many $ try blankLn
       liftM2 (Ast depth)
                  (try $ nodeFirstLn depth) -- Node
                  (children $ depth+1)      -- [Ast]

nodeFirstLn :: Int -> Parser Node
nodeFirstLn depth =
    do indent >> notFollowedBy space
       choice [ escapedLn
              , try htmlComment
              , try codeComment
              , silentJS
              , noisyJS
              , textLn
              , tag
              ]
    where indent = string $ take (depth*2) $ repeat ' '

escapedLn :: Parser Node
escapedLn = char '\\' >> liftM Text tillEol

textLn :: Parser Node
textLn =
    liftM Text $ liftM2 (:) (noneOf tagInitChs) tillEol
    where tagInitChs = "%#."
