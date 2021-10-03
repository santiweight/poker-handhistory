module Poker.History.Bovada.Model where

import           Data.Text                      ( Text )
import           Poker
import GHC.Generics
import Data.Time
import Data.Map.Strict (Map)
import GHC.TypeLits (Symbol)

data PlayerAction t = PlayerAction
  { position :: !Position
  , action   :: !(BetAction t)
  }
  deriving (Read, Show, Eq, Ord, Functor)

-- data TableAction t
  -- = TableAction Position (TableActionValue t)
  -- UnknownAction
  -- deriving (Read, Show, Eq, Ord, Functor)

data TableActionValue t
  = Post !t
  -- | PlayerSaid Text
  | PostDead !t
  | Leave
  -- | Join !Seat
  | Deposit !t
  | SeatStand
  | Enter
  | SitOut
  -- | SittingOut
  -- | TimeOut
  -- | IsDisconnected
  -- | IsConnected
  -- | TimeOutWhileDisconnected
  -- TODO remove, probably subsumed by another action
  -- | AllowedToPlayerAfterButton
  -- | FailToPost
  | SitDown
  -- TODO extract showdown contents (not sure if they exist)
  | UnknownShowdown
  -- TODO parse full information
  | SeatSummary
  | Showdown ![Card] !Text
  -- | Muck ![Card] !Text
  | Rejoin
  | Return !t
  | Result !t
  deriving (Read, Show, Ord, Eq, Functor)

-- TODO Fix the below to become the above
data DealerAction =
  PlayerDeal
  | FlopDeal !Card !Card !Card
  | TurnDeal !Card
  | RiverDeal !Card
  deriving (Read, Show, Eq, Ord)

data TableAction t
  = KnownPlayer !Position !(TableActionValue t)
  | UnknownPlayer !(TableActionValue t)
  deriving (Read, Show, Eq, Ord, Functor)

data Action t
  = MkBetAction !Position !(BetAction t)
  | MkDealerAction !DealerAction
  | MkTableAction !(TableAction t)
  deriving (Read, Show, Eq, Ord, Functor)

data GameType = Zone | Cash
  deriving (Show, Eq, Ord, Read, Enum, Generic)

data Player t = Player
  { _playerHolding :: !(Maybe Hand)
  , _stack         :: !t -- TODO use newtype
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Network = Bovada | PokerStars | Unknown
  deriving (Read, Show, Enum, Eq, Ord, Generic)

data Header = Header
  { gameId :: !Int
  , gameTy :: !GameType
  , time   :: !LocalTime
  }
  deriving (Show, Eq, Ord, Generic)

data History b = History
  { header         :: !Header
  , _handStakes    :: !(Stake b)
  , _handPlayerMap :: !(Map Seat (Player b))
  -- , _handPositionMap :: !(Map Position (Player b))
  , _handSeatMap   :: !(Map Position Seat)
  , _handActions   :: ![Action b]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Curr (c :: Symbol) where
  USD ::Curr "USD"
  EUR ::Curr "EUR"
  GBP ::Curr "GBP"

data SomeHistory where
  SomeHistory ::Curr c -> History (Amount c) -> SomeHistory
deriving instance Show (Curr c)

data SomeCurr where
  SomeCurr ::{unSomeCurr :: Curr c} -> SomeCurr

data SomeBetSize where
  SomeBetSize ::Curr c -> Rational -> SomeBetSize

data BetSize c = BetSize !(Curr c) Rational

deriving instance Show SomeBetSize
