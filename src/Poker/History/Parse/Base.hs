{-# LANGUAGE OverloadedStrings #-}

module Poker.History.Parse.Base where

import Data.Void (Void)
import Text.Megaparsec
import Data.Functor.Identity (Identity)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Data.Text (Text)
import Control.Monad (void)

-- | Parsing Monad
-- TODO change the error component
type Parser a = ParsecT Void Text Identity a

-- default space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- default space consuming lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme_ :: Parser a -> Parser ()
lexeme_ = void . lexeme

string_ :: Text -> Parser ()
string_ = void . string

-- integer matcher
integer :: Parser Int
integer = lexeme L.decimal <?> "integer"

-- rational matcher
rational :: Parser Rational
rational = toRational <$> L.scientific <?> "rational"

try_ :: Parser a -> Parser ()
try_ = void . try

optional_ :: Parser a -> Parser ()
optional_ = void . optional

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ = void . symbol

parens, braces, angles, brackets :: Parser a -> Parser a
parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
angles    = between (symbol "<") (symbol ">")
brackets  = between (symbol "[") (symbol "]")

semicolon, comma, colon, dot :: Parser Text
semicolon = symbol ";"
comma     = symbol ","
colon     = symbol ":"
dot       = symbol "."