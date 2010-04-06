module Jaml.Types where

data Ast = Ast Depth Node [Ast]
           deriving (Show)

type Depth = Int

-- FixMe: Make args into [Attr]
data Node = TemplateTag TemplateClasses TemplateId TemplateArgs
          | HtmlComment String
          | CodeComment String
          | IeConditionalComment String
          | SilentJS String
          | NoisyJS String
          | Text String
          | Tag String [Attr] (Gator,Gator) TagContent
            deriving (Show)

type TemplateArgs    = [(String,String)]
type TemplateId      = Attr
type TemplateClasses = [Attr]

data TagContent = EmptyContent
                | NoisyCode String
                | PlainText String
                | SelfClosing
                  deriving (Show)

data Gator = OutGator | InGator | NoGator
             deriving (Show, Eq)

data Attr = Attr (String, [AttrVal])
            deriving (Show)

data AttrVal = JS String
             | Literal String
               deriving (Show)

