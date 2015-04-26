{-# LANGUAGE FlexibleContexts #-}

module Lexer 
    (parse) where

import qualified Text.Parsec as P


data Token = StringLiteral String deriving Show

-- Parse a name
-- name := letter alphaNum*
name :: P.Stream s m Char => P.ParsecT s u m String
name = do
    start <- P.letter
    rest <- P.many P.alphaNum
    return $ [start]++rest




parse :: String -> IO ()
parse s = P.parseTest name s
