module Poker.History.Bovada.Model where

import           Data.Text                      ( Text )
import           Poker
import GHC.Generics
import Data.Time
import Data.Map.Strict (Map)

data PlayerAction t = PlayerAction
  { position :: !Position
  , action   :: !(BetAction t)
  }
  deriving (Read, Show, Eq, Ord, Functor)

data TableActionValue t
  = Post !t
  | PostDead !t
  | Leave
  | Deposit !t
  | SeatStand
  | Enter
  | SitOut
  | SitDown
  | UnknownShowdown
  | SeatSummary
  | Showdown ![Card] !Text
  | Rejoin
  | Return !t
  | Result !t
  deriving (Read, Show, Ord, Eq, Functor)

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
  , _stack         :: !t -- TODO use Stack
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
  , _handSeatMap   :: !(Map Position Seat)
  , _handActions   :: ![Action b]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

