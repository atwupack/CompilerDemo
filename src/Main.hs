module Main where

import Lexer

main = do
    result <- parse "data/test.txt"
    case result of
        Left err -> print err
        Right tokens -> print tokens
