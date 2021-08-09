{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module Poker.History.Types where

import           Data.Map.Strict                ( Map )
import           Data.Time                      ( LocalTime )
import           GHC.Generics                   ( Generic )
import           Poker.Base
import GHC.TypeLits (Symbol)
import Data.Text (Text)

data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Enum, Generic)

data Player t
  = Player
      { _name :: !(Maybe String)
      , _playerPosition :: !(Maybe Position)
      , _playerHolding :: !(Maybe Hand)
      , _stack :: !t
      , _seat :: !Seat
      }
  deriving (Show, Eq, Ord, Generic, Functor)

data Network = Bovada | PokerStars | Unknown
  deriving (Read, Show, Enum, Eq, Ord, Generic)

data History c = History
  { _handID        :: !Int
  , _handNetwork   :: !Network
  , _handTy        :: !GameType
  , _handTime      :: !LocalTime
  , _handStakes    :: !(Stake c)
  , _handPlayerMap :: !(Map Seat (Player c))
  , _handSeatMap   :: !(Map Position Seat)
  , _handActions   :: ![Action c]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Curr (c :: Symbol) where
  USD ::Curr "USD"
  EUR ::Curr "EUR"
  GBP ::Curr "GBP"

deriving instance Show (Curr c)

data SomeCurr where
  SomeCurr :: {unSomeCurr :: Curr c} -> SomeCurr

data SomeBetSize where
  SomeBetSize :: Curr c -> Rational -> SomeBetSize

deriving instance Show SomeBetSize
