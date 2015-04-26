{-# LANGUAGE FlexibleContexts #-}

module Lexer (
    tokenizeFile,
    Token
) where

import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as PS
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
data Keyword = Keyword String Token deriving Show

-- All valid keywords
allKeywords = [Keyword "import" Import]

-- Parse a keyword
keyword ::  P.Stream s m Char => Keyword -> P.ParsecT s u m Token
keyword (Keyword n t) = do
    s <- P.string n
    return t

-- Parse any keyword
anyKeyword :: P.Stream s m Char => P.ParsecT s u m Token
anyKeyword = P.choice $ map keyword allKeywords

-- Parse a name
-- name := letter alphaNum*
name :: P.Stream s m Char => P.ParsecT s u m String
name = do
    start <- P.letter
    rest <- P.many P.alphaNum
    return $ start : rest

-- Parse an annotation
-- annotation :: '@' name
annotation :: P.Stream s m Char => P.ParsecT s u m Token
annotation = do
    at <- P.char '@'
    Annotation <$> (name P.<?> "name for the annotation")

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
    P.choice [annotation, directive, anyKeyword, identifier]

-- Tokenize a stream of characters into tokens
tokenize :: P.Stream s m Char => P.ParsecT s u m [Token]
tokenize = P.many token

tokenizeFile :: String -> IO (Either P.ParseError [Token]) 
tokenizeFile f = PS.parseFromFile tokenize f
