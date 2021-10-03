module Poker.History.Types where

import GHC.TypeLits (Symbol, KnownSymbol, sameSymbol)
import Data.Proxy


data Curr (c :: Symbol) where
  USD :: Curr "USD"
  EUR :: Curr "EUR"
  GBP :: Curr "GBP"

data SomeCurr where
  SomeCurr :: forall (c :: Symbol). KnownSymbol c => {unSomeCurr :: Curr c} -> SomeCurr

deriving instance Eq (Curr c)

deriving instance Show (Curr c)

data SomeBetSize where
  SomeBetSize ::KnownSymbol c => Curr c -> Rational -> SomeBetSize

data BetSize c = BetSize !(Curr c) Rational

deriving instance Show SomeBetSize

instance Eq SomeBetSize where
  (SomeBetSize (cu :: Curr c1) b1) == (SomeBetSize (cu' :: Curr c2) b2) =
    case sameSymbol (Proxy @c1) (Proxy @c2) of
      Nothing -> False
      Just _  -> b1 == b2
