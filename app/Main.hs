module Main where

import Parser
import Text.ParserCombinators.Parsec
import System.Environment

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    print $ parse parseINI "" contents
