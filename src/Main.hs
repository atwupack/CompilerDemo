module Main where

import Lexer

main = do
    result <- tokenizeFile "data/test.txt"
    case result of
        Left err -> print err
        Right tokens -> print tokens
