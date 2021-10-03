{-# Language OverloadedStrings #-}

module Poker.History.Base where
import Text.Megaparsec
import Data.Void
import Data.Text (Text)
import Control.Monad.Identity
import qualified Text.Megaparsec.Char.Lexer as L
import Poker.History.Types
import Text.Megaparsec.Char
import Poker
import Text.Printf (printf)
import qualified Data.Text as T

-- | Parsing Monad
-- TODO change the error component
type Parser a = ParsecT Void Text Identity a

-- default space consumer
sc :: MonadParsec Void Text m => m ()
sc = L.space space1 empty empty

-- default space consuming lexeme
lexeme :: MonadParsec Void Text m => m a -> m a
lexeme = L.lexeme sc

lexeme_ :: MonadParsec Void Text m => m a -> m ()
lexeme_ = void . lexeme

string_ :: MonadParsec Void Text m => Text -> m ()
string_ = void . string

-- integer matcher
integer :: MonadParsec Void Text m => m Int
integer = lexeme L.decimal <?> "integer"

-- rational matcher
rational :: MonadParsec Void Text m => m Rational
rational = toRational <$> L.scientific <?> "rational"

try_ :: MonadParsec Void Text m => m a -> m ()
try_ = void . try

optional_ :: MonadParsec Void Text m => m a -> m ()
optional_ = void . optional

symbol :: MonadParsec Void Text m => Text -> m Text
symbol = L.symbol sc

symbol_ :: MonadParsec Void Text m => Text -> m ()
symbol_ = void . symbol

parens, braces, angles, brackets, singleQuotes
  :: MonadParsec Void Text m => m a -> m a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
angles = between (symbol "<") (symbol ">")
brackets = between (symbol "[") (symbol "]")
singleQuotes = between (symbol "'") (symbol "'")

semicolon, comma, colon, dot, fwdSlash :: MonadParsec Void Text m => m Text
semicolon = symbol ";"
comma = symbol ","
colon = symbol ":"
dot = symbol "."
fwdSlash = symbol "/"

pCurrency :: MonadParsec Void Text m => m SomeCurr
pCurrency = anySingle >>= \case
  '$' -> pure $ SomeCurr USD
  '€' -> pure $ SomeCurr EUR
  '£' -> pure $ SomeCurr GBP
  _   -> empty <?> "currency"

pCard :: MonadParsec Void Text m => m Card
pCard = parsePrettyP

eol_ :: MonadParsec e Text m => m ()
eol_ = void eol

line_ :: MonadParsec Void Text m => m a -> m ()
line_ = (>> eol_)

line :: MonadParsec Void Text m => m a -> m a
line = (<* eol_)

-- | Match a specified number of cards
countCard :: MonadParsec Void Text m => Int -> m () -> m [Card]
countCard num pBetween = do
  cards <- try pCard `sepEndBy` pBetween <?> "Multiple Cards"
  if length cards /= num
    then empty <?> printf "Expected %d cards, but found %d" num (length cards)
    else pure cards

-- TODO change this to pCurrencyAmount
-- TODO parse pCurrency type
pAmount :: MonadParsec Void Text m => m SomeBetSize
pAmount =
  lexeme
    ( pCurrency >>= \case
        SomeCurr curr -> SomeBetSize curr <$> rational
    )

lineEndedBy :: (MonadParsec e Text m) => Text -> m Text
lineEndedBy suffix = do
  (T.pack -> lineTxt) <- many (notFollowedBy eol *> anySingle)
  guard $ suffix `T.isSuffixOf` lineTxt
  eol_
  pure lineTxt
