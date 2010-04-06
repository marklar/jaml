module Jaml.Generator.Ast
( astToJS
) where

import Text.Regex.Posix

import Jaml.Types
import Jaml.Generator.Util (bite)
import Jaml.Generator.Node (nodeToJS)

{-
  All Generator funs generate (return) Strings of JavaScript.
-}

astToJS :: String -> Ast -> String
astToJS idPrefix (Ast depth node kids) =
    case node of CodeComment _ -> ""
                 otherwise -> concat [ indent
                                     , nodeToJS idPrefix node
                                     , postOpen node kids
                                     , "\n"
                                     , concat $ map (astToJS idPrefix) kids
                                     , close node indent kids
                                     ]
    where indent = take (2*depth) (repeat ' ')

{--------------------------}
{- At end of node's line. -}
postOpen :: Node -> [Ast] -> String

-- Beginning of JS function.
postOpen (TemplateTag _ _ _) _  =  " {\n  var _J=Jaml(), _j=new _J();"

-- Whether to put whitespace around innerHTML.
postOpen (Tag _ _ (_,InGator) _) _  =  bite
postOpen (Tag _ _ _           _) _  =  ""

-- If SINGLE line of JS lacks final ';', add one.
postOpen (SilentJS s) [] =  if s =~ ";[:blank:]*$"   then "" else ";"
-- If start of MULTIPLE lines of JS, and lacks final '{', add one.
postOpen (SilentJS s) _  =  if s =~ "\\{[:blank:]*$" then "" else " {"

-- *Comment, NoisyJs, Text
postOpen _ _ = ""


{-------------------------------------}
{- Close node (unless self-closing). -}
close :: Node -> String -> [Ast] -> String

-- End of JS function.
close (TemplateTag _ _ _) _ _            = "  return _j.v();\n};\n"

-- If MULTIPLE lines of JS, close the block.
close (SilentJS _) _      []             = ""
close (SilentJS _) indent _              = indent ++ "}\n"

-- HTML comments can be multiline.
close (HtmlComment _)          indent _  = indent ++ "_j.s('-->');\n"
close (IeConditionalComment _) indent _  = indent ++ "_j.s('<![endif]-->');\n"

-- Add gator "bite(s)" if necessary.
close (Tag _    _ (OutGator,_) SelfClosing) _      _ = bite
close (Tag _    _ _            SelfClosing) _      _ = ""
close (Tag name _ (outG,inG)   _          ) indent _ =
    concat [ indent
           , if inG  == InGator  then bite else ""
           , "_j.ns('</" ++ name ++ ">');"
           , if outG == OutGator then bite else ""
           , "\n"
           ]

-- CodeComment, NoisyJs, Text
close _ _ _ = ""
