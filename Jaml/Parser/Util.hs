module Jaml.Parser.Util where

import Text.ParserCombinators.Parsec
import Control.Monad (liftM, liftM2)

{-- Fully Applied Parsers --}

inQuotes :: Parser String
inQuotes = stringInside '\'' <|> stringInside '"'

blankLn :: Parser String
blankLn = manyTill nonNewlineSpace newline

tillEol :: Parser String
tillEol = manyTill anyChar newline

-- want to use skipMany instead?
optSpaces :: Parser String
optSpaces = many nonNewlineSpace

-- identifiers --
identifier :: Parser String =
    liftM2 (:) (oneOf idPreChars <?> "letter or '_' at start of identifier")
               (many $ oneOf idChars)

{-- Combinators --}

-- EXclusive
stringInside :: Char -> Parser String
stringInside c =
    between (char c) (char c) (many $ noneOf [c])

-- INclusive
allBounded :: String -> String -> Parser String
allBounded pre post =
    liftM (\s -> pre++s++post) $ allBetween pre post 

-- EXclusive
allBetween :: String -> String -> Parser String
allBetween pre post =
    between (string pre) (string post) (many $ noneOf post)

{- I struggled with this name.
   It doesn't take a String and trim it.
   It's a parser, of course.
   It expects a String that may have trimmable whitespace
   and returns the trimmed String.
-}
trimmed :: Parser String -> Parser String
trimmed = between optSpaces optSpaces


{-- Not exported --}

nonNewlineSpace :: Parser Char
nonNewlineSpace = oneOf " \v\f\t"

-- at top level so they're computed only once
idPreChars :: [Char] = ['a'..'z']++['A'..'Z']++"_"
idChars :: [Char] = idPreChars++['0'..'9']
