{-# LANGUAGE EmptyDataDeriving #-}
module Poker.History.PokerStars.Model where

import           Data.Text                      ( Text )
import           Poker
import GHC.Generics
import Data.Time
import Data.Map.Strict (Map)

data TableActionValue t
  = PlayerSaid Text
  | Post !t
  | PostDead !t
  | PostSuperDead !t
  | Ante !t
  | Leave
  | Join !Seat
  | SitOut
  | SittingOut
  | TimeOut
  | IsDisconnected
  | IsConnected
  | TimeOutWhileDisconnected
  -- TODO remove, probably subsumed by another action
  | AllowedToPlayerAfterButton
  | FailToPost
  deriving (Read, Show, Ord, Eq, Functor)

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
  , _stack :: !t -- TODO use Stack
  }
  deriving (Show, Eq, Ord, Generic, Functor)

data Header = Header
  { gameId   :: !Int
  , gameTy   :: !GameType
  , time     :: !LocalTime
  }
  deriving (Show, Eq, Ord, Generic)

data History b = History
  { header         :: !Header
  , _handStakes    :: !(Stake b)
  , _handPlayerMap :: !(Map Seat (Player b))
  , _handSeatMap   :: !(Map Seat Position)
  , _handActions   :: ![Action b]
  , _handText      :: !Text
  }
  deriving (Show, Eq, Ord, Generic, Functor)

