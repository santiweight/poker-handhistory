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
type Parser a = Parsec Void Text a

-- default space consumer
sc :: Parser ()
sc = L.space space1 empty empty

-- default space consuming lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme_ :: ParsecT Void Text Identity a -> Parser ()
lexeme_ = void . lexeme

string_ :: Text -> ParsecT Void Text Identity ()
string_ = void . string

inParens :: Parser a -> Parser a
inParens = between (char '(') (char ')')

-- integer matcher
integer :: Parser Int
integer = lexeme L.decimal <?> "integer"

-- rational matcher
rational :: Parser Rational
rational = toRational <$> L.scientific <?> "rational"

-- wrap with spaceconsumer
-- TODO this is ugly and symptomatic of bad stuff later
spaceWrap :: Parser a -> Parser a
spaceWrap parser = space *> parser <* many (char ' ')

-- surround an expression in brackets
inBrackets :: Parser a -> Parser a
inBrackets = between (char '[') (char ']')

-- match a colon
colon :: Parser Text
colon = L.symbol sc ":" <?> "colon"

try_ :: Parser a -> Parser ()
try_ = void . try

optional_ :: Parser a -> Parser ()
optional_ = void . optional