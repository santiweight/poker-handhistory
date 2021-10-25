{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Poker.History.Bovada.Parser (pHand, pHands) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad
import Data.Foldable (Foldable (toList))
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromMaybe,
  )
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime
  ( LocalTime (..),
    TimeOfDay (..),
  )
import Poker
import Poker.History.Base
import Poker.History.Bovada.Model
import Poker.History.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (id)

-- | Primitive Combinators

-- Parse some 'Card's separated by a separator-consuming parser
manyCardsP ::
  -- | Parse that consumes between 'Card's
  Parser () ->
  Parser [Card]
manyCardsP pBetween =
  try (try pCard `sepEndBy` try pBetween) <?> "Multiple Cards"

-- parses a Position type
pPosition :: Parser (Position, IsHero)
pPosition = liftA2 (,) pPos pHero
  where
    pHero = option Villain $ Hero <$ try (sc *> "[ME] ")
    pPos = pPosString <* spaceChar
    pPosString =
      label "Position" $
        choice
          [ SB <$ "Small Blind",
            BB <$ "Big Blind",
            UTG1 <$ "UTG+1",
            UTG2 <$ "UTG+2",
            UTG <$ "UTG",
            BU <$ "Dealer"
          ]

-- TODO make this return a more complex datatype HandHeader
-- TODO accept multiple hand header types
-- TODO match specific network based on header
-- TODO find out if it's yyyy/mm/dd or yyyy/dd/mm
-- TODO figure out time format
-- TODO match different game types eg PLO
handHeaderP :: Parser Header
handHeaderP = do
  handId <- "Bovada Hand #" *> integer <* " " <?> "Hand ID"
  zoneMay <- optional $ Zone <$ " Zone Poker "
  optional_ $ "ID#" *> integer <* " "
  optional_ $ "TBL#" *> integer <* " "
  "HOLDEM" >> optional "ZonePoker" >> " No Limit - "
  let tuple3Sep pBetween p = liftM3 (,,) (p <* pBetween) (p <* pBetween) p
  (year, month, day) <- tuple3Sep dash integer <* " "
  (hour, minute, second) <- tuple3Sep colon integer <* eol
  let date = fromGregorian (fromIntegral year) month day
  let timeOfDay = TimeOfDay hour minute (fromIntegral second)
  let localTime = LocalTime date timeOfDay
  pure
    Header
      { gameId = handId,
        gameTy = fromMaybe Cash zoneMay,
        time = localTime
      }

pActionValue :: Parser (BetAction SomeBetSize)
pActionValue =
  choice
    . fmap try
    $ [pComplexFold, pFold, pCheck, pCall, pRaise, pBet, pAllInRaise, pAllIn]
  where
    pFold =
      ((Fold <$) . choice . fmap (string . ("Folds" <>)))
        [" (timeout)", " (disconnect)", " (auth)", ""]
    pComplexFold =
      Fold <$ choice ["Folds & shows " >> brackets (manyCardsP sc)]
    pCheck :: Parser (BetAction SomeBetSize)
    pCheck =
      Check
        <$ (choice . fmap (string . ("Checks" <>)))
          [" (timeout)", " (disconnect)", ""]
    pCall = Call <$> ("Calls " *> pAmount)
    pRaise = uncurry Raise <$> ("Raises " >> pAmountFromTo)
    pBet = Bet <$> ("Bets " *> pAmount)
    pAllInRaise =
      uncurry AllInRaise <$> ("All-in(raise) " >> pAmountFromTo)
    pAllIn = AllIn <$> ("All-in " *> pAmount)
    pAmountFromTo = liftM2 (,) pAmount ("to " *> pAmount)

-- pDealer matches dealer announcements and exhibits how awful Bovada's format is
pDealer :: Parser ()
pDealer = try pSetDealerPosition <|> "Set dealer" *> eol_
  where
    pSetDealerPosition = do
      "Dealer " >> optional_ " [ME] "
      void $ pSetDealerTxt *> brackets integer
    pSetDealerTxt = ": Set dealer " <|> ": Set deale  r "

pSmallBlind :: Parser (Maybe (TableAction SomeBetSize))
pSmallBlind = label "Small blind post" . optional $ do
  p <- pSmallBlindPosition <* optional_ "[ME] "
  ": Small Blind " >> mkPost p <$> pAmount
  where
    pSmallBlindPosition = SB <$ symbol "Small Blind" <|> BU <$ symbol "Dealer"
    mkPost p = KnownPlayer p . Post

pBigBlind :: Parser (TableAction SomeBetSize, SomeBetSize)
pBigBlind = label "Big blind post" $ do
  "Big Blind " >> optional_ " [ME] " >> ": Big blind "
  bb <- pAmount
  pure (mkBBPost bb, bb)
  where
    mkBBPost = KnownPlayer BB . Post

pFlop :: Parser (Action t)
pFlop = do
  "*** FLOP *** "
  [c1, c2, c3] <- brackets (countCard 3 sc)
  pure . MkDealerAction $ FlopDeal c1 c2 c3

turnStreetP :: Parser (Action t)
turnStreetP = do
  "*** TURN *** "
  void $ brackets (countCard 3 sc)
  MkDealerAction . TurnDeal <$> brackets pCard

riverStreetP :: Parser (Action t)
riverStreetP = do
  "*** RIVER *** "
  void $ brackets (countCard 4 sc)
  MkDealerAction . RiverDeal <$> brackets pCard

pHoldingsMap :: Parser (DealerAction, Map Position Hole)
pHoldingsMap = label "Card deal" $ do
  symbol_ "*** HOLE CARDS ***"
  holdingMap <- M.fromList <$> many (try pDeal)
  pure (PlayerDeal, holdingMap)
  where
    pDeal = do
      (p, _) <- pPosition
      ": Card dealt to a spot "
      h <- brackets $ liftM2 unsafeHole (pCard <* spaceChar) pCard
      pure (p, h)

pStack :: Parser (Int, Position, SomeBetSize, IsHero)
pStack = do
  seat <- "Seat " *> decimal <* colon
  (pos, hero) <- pPosition
  stack <- parens (pAmount <* "in chips")
  pure (seat, pos, stack, hero)

pSummary :: Parser ()
pSummary = void "*** SUMMARY ***"

-- TODO many calls to UnknownPlayer are incorrect here. We may be able to get more position information
pTableAction :: Parser (TableAction SomeBetSize) -- TableAction
pTableAction =
  (lexeme . choice . fmap try)
    [ UnknownPlayer <$> pSeatStand,
      UnknownPlayer <$> pShowdown,
      pKnownTableAction,
      UnknownPlayer <$> pSimpleUnknown,
      UnknownPlayer <$> pTableDeposit
    ]
  where
    pKnownTableAction = do
      (pos, _) <- pPosition <* colon
      tableActionVal <-
        choice
          . fmap try
          $ [ pDeposit,
              pSitDown,
              pSitOut,
              pMuck,
              pResult,
              pLeave,
              pRejoin,
              pEnter,
              pReturnUncalled,
              pTableDeposit
            ]
      pure $ KnownPlayer pos tableActionVal
    pShowdown =
      UnknownShowdown <$ maybePositioned_ "Showdown(High Card)"
    pSeatStand = SeatStand <$ maybePositioned_ "Seat stand"
    pSimpleUnknown =
      try pTableLeaveOrEnter <|> try pSeatUpdate
        <|> choice
          [Enter <$ "Enter(Auto)", Leave <$ "Leave(Auto)"]

    pReturnUncalled =
      Return <$> ("Return uncalled portion of bet " >> pAmount)
    pResult =
      Result
        <$> ( choice ["Hand result-Side pot ", "Hand result "]
                *> pAmount
            )
    pMuck = do
      showdownStr <-
        choice ["Mucks ", "Does not show ", "Showdown "]
      cards <- brackets (manyCardsP sc)
      (sc <* (parens . many) (lexeme letterChar))
        <|> void (many printChar)
      pure $ Showdown cards showdownStr
    pLeave =
      Leave <$ choice ["Leave(Auto)", "Table leave user"]
    pEnter =
      Enter <$ choice ["Enter(Auto)", "Table enter user"]
    pRejoin = Rejoin <$ "Seat re-join"
    pDeposit = Deposit <$> ("Table deposit " *> pAmount)
    pSitDown = SitDown <$ "Seat sit down"
    pSitOut = SitOut <$ "Seat sit out"

pTableLeaveOrEnter :: Parser (TableActionValue t)
pTableLeaveOrEnter =
  choice
    . fmap try
    $ [Leave <$ symbol "Table leave user", Enter <$ "Table enter user"]

pPost :: Parser (TableAction SomeBetSize)
pPost = do
  (p, _) <- pPosition <* colon
  tableActVal <-
    choice
      [ PostDead <$> ("Posts dead chip " >> pAmount),
        PostDead <$> (("Posts chip " <|> "Posts ") *> pAmount)
      ]
  pure $ KnownPlayer p tableActVal

-- pAction parses a simple player action
pAction :: Parser (Action SomeBetSize)
pAction =
  lexeme $
    try
      ( do
          (pos, _) <- pPosition <* colon
          -- TODO track isHero
          MkBetAction pos <$> pActionValue
      )
      <|> MkTableAction
      <$> try pTableAction

pStacks :: Parser [(Int, Position, SomeBetSize, IsHero)]
pStacks = some pStack

getPlayers ::
  [(Int, Position, t, d)] ->
  Map Position Hole ->
  Map Seat (Seat, Position, Player t)
getPlayers stacks holdings =
  let players = M.fromList $ do
        (seat_, pos_, stack_, _) <- stacks
        pure $
          (,)
            (Seat seat_)
            ( Seat seat_,
              pos_,
              Player
                { _holding = M.lookup pos_ holdings,
                  _stack = Stack stack_
                }
            )
   in players

street :: Parser (Action SomeBetSize) -> Parser [Action SomeBetSize]
street streetHeader = liftM2 (:) streetHeader (many $ try pAction)

getSeatMap :: Map Seat (Seat, Maybe Position, Player t) -> Map Position Seat
getSeatMap players =
  let func (seat, posMay, _) seatMap' = case posMay of
        Nothing -> seatMap'
        Just pos -> M.insert pos seat seatMap'
   in M.foldr func Map.empty players

streets :: Parser [Action SomeBetSize]
streets = do
  preflopAs <- some pAction <?> "Preflop"
  postFlopAs <- option [] $ do
    flopAs <- street pFlop <?> "Flop"
    turnAndRiverAs <- option [] $ do
      turnAs <- street turnStreetP <?> "Turn"
      riverAs <- option [] (street riverStreetP <?> "River")
      pure $ turnAs <> riverAs
    pure $ flopAs <> turnAndRiverAs
  pure $ preflopAs <> postFlopAs

pSeatUpdate :: Parser (TableActionValue t)
pSeatUpdate =
  choice
    [ Rejoin <$ symbol "Seat re-join",
      SitOut <$ symbol "Seat sit out",
      SitDown <$ symbol "Seat sit down",
      SeatStand <$ symbol "Seat stand"
    ]

pTableDeposit :: Parser (TableActionValue SomeBetSize)
pTableDeposit = Deposit <$> ("Table deposit " >> pAmount)

pTableOrSeatLines :: Parser ()
pTableOrSeatLines =
  (void . many . choice . fmap try)
    [ maybePositioned_ pTableDeposit,
      maybePositioned_ pSeatUpdate,
      maybePositioned_ $ lexeme pTableLeaveOrEnter
    ]

maybePositioned_ :: Parser b -> Parser ()
maybePositioned_ = void . (optional (pPosition >> colon) >>)

-- handP is the primary hand parser that matches a hand
pHand :: Parser (History SomeBetSize) -- Hand
pHand = do
  handHeader <- handHeaderP
  stacks <- pStacks
  optional_ pDealer
  pTableOrSeatLines
  sbActMay <- pSmallBlind
  pTableOrSeatLines
  (bbAct, bb) <- pBigBlind
  pTableOrSeatLines
  postAs <- many . try $ pPost
  _ <- many pTableAction
  (preFlopDeal, holdings) <- pHoldingsMap
  postFlopAs <- streets
  _ <- many pTableAction
  pSummary
  -- TODO parse summary
  _ <- many (try $ notFollowedBy (lookAhead (eol >> eol)) >> anySingle)
  let allAs =
        concat
          [ toList $ MkTableAction <$> sbActMay,
            [MkTableAction bbAct],
            MkTableAction <$> postAs,
            [MkDealerAction preFlopDeal],
            postFlopAs
          ]
  let players = getPlayers stacks holdings
  pure
    $! History
      { header = handHeader,
        _handStakes = Stake bb,
        _handActions = allAs,
        _handPlayerMap = Map.map (\(_, _, c) -> c) players,
        _handSeatMap = getSeatMap $ Map.map (\(a, b, c) -> (a, Just b, c)) players,
        _handText = ""
      }

-- | Parser that can extract 'History's from a 'Text'. Can parse 0 'History's if the
-- 'Text' contents are empty.
pHands :: Parser [History SomeBetSize]
pHands = between sc eof $ do
  res <- many (lexeme pHand)
  _ <- many eol
  pure res
