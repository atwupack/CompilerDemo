{-# LANGUAGE FlexibleContexts #-}

module Parser 

    (parseTokens) where

import Lexer
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Applicative
import Text.Parsec.Pos
import Text.Parsec.Error

data AstRoot = AstRoot deriving Show

satisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy f = tokenPrim showToken nextPos testToken
    where
        showToken t = show t
        nextPos pos t s = initialPos ""
        testToken t = if f t then Just t else Nothing




parseTokens :: [Token] -> Either ParseError AstRoot
parseTokens t = Right AstRoot
