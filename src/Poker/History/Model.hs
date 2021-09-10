{-# LANGUAGE EmptyDataDeriving #-}
module Poker.History.Model where

import Data.Text (Text)
import Poker

data BetAction t
  = Call !t
  | Raise
      { raiseBy :: !t,
        raiseTo :: !t
      }
  | AllInRaise
      { raiseBy :: !t, -- TODO remove?
        raiseTo :: !t
      }
  | Bet !t
  | AllIn !t
  | Fold
  | Check
  -- | OtherAction -- TODO remove
  deriving (Read, Show, Eq, Ord, Functor)

data PlayerAction t = PlayerAction
  { position :: !Position
  , action   :: !(BetAction t)
  }
  deriving (Read, Show, Eq, Ord, Functor)

-- data TableAction t
  -- = TableAction Position (TableActionValue t)
  -- | UnknownAction
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
  -- -- TODO remove, probably subsumed by another action
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

