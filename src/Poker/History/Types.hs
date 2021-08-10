{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Poker.History.Types where

import           Data.Map.Strict                ( Map )
import           Data.Text                      ( Text )
import           Data.Time                      ( LocalTime )
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits                   ( Symbol )
import           Poker.Base

data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Enum, Generic)

data Player t = Player
  { _name           :: !(Maybe String)
  , _playerPosition :: !(Maybe Position)
  , _playerHolding  :: !(Maybe Hand)
  , _stack          :: !t
  , _seat           :: !Seat
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Network = Bovada | PokerStars | Unknown
  deriving (Read, Show, Enum, Eq, Ord, Generic)

data SNetwork net where
  SBovada ::SNetwork Bovada
  SPokerStars ::SNetwork PokerStars

deriving instance Show (SNetwork net)
deriving instance Eq (SNetwork net)
deriving instance Ord (SNetwork net)

data Header net = Header
  { sNetwork :: !(SNetwork net)
  , gameId       :: !Int
  , gameTy   :: !GameType
  , time     :: !LocalTime
  }
  deriving (Show, Eq, Ord, Generic)

data History net b = History
  { header         :: Header net
  , _handStakes    :: !(Stake b)
  , _handPlayerMap :: !(Map Seat (Player b))
  , _handSeatMap   :: !(Map Position Seat)
  , _handActions   :: ![Action b]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Curr (c :: Symbol) where
  USD ::Curr "USD"
  EUR ::Curr "EUR"
  GBP ::Curr "GBP"

deriving instance Show (Curr c)

data SomeCurr where
  SomeCurr ::{unSomeCurr :: Curr c} -> SomeCurr

data SomeBetSize where
  SomeBetSize ::Curr c -> Rational -> SomeBetSize

deriving instance Show SomeBetSize
