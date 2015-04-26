{-# LANGUAGE FlexibleContexts #-}

module Lexer 
    (parse) where

import qualified Text.Parsec as P
import Control.Applicative

-- The valid tokens in the language
data Token =
    StringLiteral String
    | Identifier String
    | Annotation String
    | CompilerDirective String
    | Import
    deriving Show

-- Data for all the keywords in the language
-data Keyword = Keyword String Token deriving Show

-- All valid keywords
--keywords = [Keyword "import" Import]

-- Parse any keyword
--keyword ::  P.Stream s m Char => P.ParsecT s u m Token
--keyword = 

-- Parse a name
-- name := letter alphaNum*
name :: P.Stream s m Char => P.ParsecT s u m String
name = do
    start <- P.letter
    rest <- P.many P.alphaNum
    return $ [start]++rest

-- Parse an annotation
-- annotation :: '@' name
annotation :: P.Stream s m Char => P.ParsecT s u m Token
annotation = do
    at <- P.char '@'
    Annotation <$> name

-- Parse a compiler directive
-- directive :: '#' name
directive :: P.Stream s m Char => P.ParsecT s u m Token
directive = do
    at <- P.char '#'
    CompilerDirective <$> name

-- Parse an identifier
-- identifier :: name
identifier :: P.Stream s m Char => P.ParsecT s u m Token
identifier =  Identifier <$> name

-- Parse any valid token
token ::  P.Stream s m Char => P.ParsecT s u m Token
token = do
    P.skipMany P.space
    P.choice [annotation, directive, identifier]

-- Tokenize a stream of characters into tokens
tokenize :: P.Stream s m Char => P.ParsecT s u m [Token]
tokenize = P.many token

parse :: String -> IO ()
parse s = P.parseTest tokenize s
