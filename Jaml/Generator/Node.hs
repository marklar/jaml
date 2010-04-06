module Jaml.Generator.Node
( nodeToJS
) where

import Jaml.Types
import Jaml.Util (joinStrs)
import Jaml.Generator.Util (bite, escSingles)
import Jaml.Generator.Attribute (attrToJS, attrValStr)

nodeToJS :: String -> Node -> String

nodeToJS _ (TemplateTag classAttrs (Attr ("id", [Literal id])) kvs) =
    className ++ ".prototype." ++ id ++ "=function(" ++ argsStr ++ ")"
    where className = joinStrs "." $ map (concat . getStrs) classAttrs
          getStrs (Attr (_, vals)) = map attrValStr vals
          argsStr = case kvs of (_,v):_ -> v  -- [JS var names]
                                []      -> ""

nodeToJS _ (IeConditionalComment s)  = "_j.s('<!--[if IE]>');_j.s('" ++ escSingles s ++ "');"
nodeToJS _ (HtmlComment s)           = "_j.s('<!-- ');_j.s('"        ++ escSingles s ++ "');"
nodeToJS _ (CodeComment s)           = "// " ++ s   -- Unused.  Such blocks not displayed.

nodeToJS _ (SilentJS s) = s
nodeToJS _ (NoisyJS s)  = "_j.ns(" ++ s ++ ");"

nodeToJS _ (Text s)     = "_j.ns('" ++ escSingles s ++ "');"

nodeToJS idPrefix (Tag name attrs (outG,inG) content) =
    concat [ if outG == OutGator then bite else ""
           , "_j.ns('<" ++ name ++ "');"
           , concat $ map (attrToJS idPrefix) attrs
           , contentStr
           ]
    where
      contentStr =
          case content of
            SelfClosing                  -> "_j.s(' />');"
            EmptyContent                 -> "_j.s('>');"
            NoisyCode s | inG == InGator -> "_j.s('>');" ++ bite ++ "_j.s(" ++ s ++ ");"
            NoisyCode s                  -> "_j.s('>'+(" ++ s ++ "));"
            PlainText s | inG == InGator -> "_j.s('>');" ++ bite ++ "_j.s('" ++ escSingles s ++ "');"
            PlainText s                  -> "_j.s('>" ++ escSingles s ++ "');"
