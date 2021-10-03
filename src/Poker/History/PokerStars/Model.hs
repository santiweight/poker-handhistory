{-# LANGUAGE EmptyDataDeriving #-}
module Poker.History.PokerStars.Model where

import           Data.Text                      ( Text )
import           Poker
import GHC.Generics
import Data.Time
import Data.Map.Strict (Map)
import GHC.TypeLits (Symbol, KnownSymbol, sameSymbol)
import Data.Proxy

data PlayerAction t = PlayerAction
  -- { position :: !Position
  -- , action   :: !(BetAction t)
  -- }
  deriving (Read, Show, Eq, Ord, Functor)

-- data TableAction t
  -- = TableAction Position (TableActionValue t)
  -- | UnknownAction
  -- deriving (Read, Show, Eq, Ord, Functor)

data TableActionValue t
  = PlayerSaid Text
  | Post !t
  -- | PostDead !t
  | Leave
  | Join !Seat
  -- | Deposit !t
  -- | Enter
  | SitOut
  | SittingOut
  | TimeOut
  | IsDisconnected
  | IsConnected
  | TimeOutWhileDisconnected
  -- TODO remove, probably subsumed by another action
  | AllowedToPlayerAfterButton
  | FailToPost
  -- | SitDown
  -- | Showdown ![Card] !Text
  -- | Muck ![Card] !Text
  -- | Rejoin
  -- | Return !t
  -- | Result !t
  deriving (Read, Show, Ord, Eq, Functor)

-- TODO Fix the below to become the above
data DealerAction =
  PlayerDeal
  | FlopDeal !Card !Card !Card
  | TurnDeal !Card
  | RiverDeal !Card
  deriving (Read, Show, Eq, Ord)

data TableAction t =
  KnownPlayer Position !(TableActionValue t)
  -- TODO get player's username
  | UnknownPlayer !(TableActionValue t)
  deriving (Read, Show, Eq, Ord, Functor)

data Action t
  = MkBetAction !Position !(BetAction t)
  | MkDealerAction !DealerAction
  | MkTableAction !(TableAction t)
  deriving (Read, Show, Eq, Ord, Functor)

data GameType = Zoom | Cash
  deriving (Show, Eq, Ord, Read, Enum, Generic)

data Player t = Player
  { _name  :: Text
  , _stack :: !t -- TODO use newtype
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
  , gameId   :: !Int
  , gameTy   :: !GameType
  , time     :: !LocalTime
  }
  deriving (Show, Eq, Ord, Generic)

data History net b = History
  { header         :: Header net
  , _handStakes    :: !(Stake b)
  , _handPlayerMap :: !(Map Seat (Player b))
  -- , _handPositionMap :: !(Map Position (Player b))
  , _handSeatMap   :: !(Map Seat Position)
  , _handActions   :: ![Action b]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Curr (c :: Symbol) where
  USD ::Curr "USD"
  EUR ::Curr "EUR"
  GBP ::Curr "GBP"

deriving instance Eq (Curr c)

data SomeHistory where
  SomeHistory ::KnownSymbol c => Curr c -> History Bovada (Amount c) -> SomeHistory
deriving instance Show (Curr c)

data SomeCurr where
  SomeCurr ::KnownSymbol c => {unSomeCurr :: Curr c} -> SomeCurr

data SomeBetSize where
  SomeBetSize ::KnownSymbol c => Curr c -> Rational -> SomeBetSize

data BetSize c = BetSize !(Curr c) Rational

deriving instance Show SomeBetSize

instance Eq SomeBetSize where
  (SomeBetSize (cu :: Curr c1) b1) == (SomeBetSize (cu' :: Curr c2) b2) =
    case sameSymbol (Proxy @c1) (Proxy @c2) of
      Nothing -> False
      Just _  -> b1 == b2
