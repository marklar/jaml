module Main where

import Text.ParserCombinators.Parsec (parseFromFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (intersperse)
import IO (putStrLn, hPutStrLn, stderr, hFlush, stdout)

import Jaml.Parser.File (jamlFile)
import Jaml.Generator.Ast (astToJS)
import Jaml.Util (joinStrs)

main =
    do (fn, pre) <- getFileNameAndIdPrefix
       errOrAsts <- parseFromFile jamlFile fn
       case errOrAsts of
         Left error -> hPutStrLn stderr (show error) >> exitFailure
         Right asts -> putStrLn $ joinStrs "\n" jsFuns
                       where jsFuns = map (astToJS pre) asts

getFileNameAndIdPrefix :: IO (FilePath,String)
getFileNameAndIdPrefix =
    do argv <- getArgs
       case argv of
         [] -> do fn <- promptLine "Please enter .jaml filename: "
                  return (fn, "")
         fn:[]    -> return (fn, "")
         fn:pre:_ -> return (fn, pre)
               
promptLine :: String -> IO String
promptLine prompt =
    do putStr prompt
       hFlush stdout
       getLine
