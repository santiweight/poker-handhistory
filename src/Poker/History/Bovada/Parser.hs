{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Poker.History.Bovada.Parser (pHand, pHands) where

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

manyCardsP :: Parser () -> Parser [Card]
manyCardsP pBetween =
  (try . lexeme) (try pCard `sepEndBy` pBetween) <?> "Multiple Cards"

-- parses a Position type
pPosition :: Parser (Position, IsHero)
pPosition = do
  p <- positionString
  hero <- option Villain $ symbol "[ME]" $> Hero
  pure (p, hero)
  where
    positionString =
      (lexeme . choice)
        [ SB <$ string "Small Blind",
          BB <$ string "Big Blind",
          UTG1 <$ string "UTG+1",
          UTG2 <$ string "UTG+2",
          UTG <$ string "UTG",
          BU <$ string "Dealer"
        ]
        <?> "expected Position"

-- TODO make this return a more complex datatype HandHeader
-- TODO accept multiple hand header types
-- TODO match specific network based on header
-- TODO find out if it's yyyy/mm/dd or yyyy/dd/mm
-- TODO figure out time format
-- TODO match different game types eg PLO
handHeaderP :: Parser Header
handHeaderP = do
  _handNetwork <-
    choice [Bovada <$ string "Bovada ", PokerStars <$ string "PokerStars "]
      <?> "Network"
  symbol_ "Hand #"
  handID_ <- integer <?> "Hand Number"
  zoneMay <- optional $ Zone <$ symbol "Zone Poker"
  _ <- optional $ symbol "ID#" *> integer
  _ <- optional $ symbol "TBL#" *> integer
  string "HOLDEM" >> optional (string "ZonePoker") >> string_ " No Limit - "
  (year_, month_, day_) <-
    lexeme $
      liftM3 (,,) (integer <* char '-') (integer <* char '-') integer
  (hour_, minute_, second_) <-
    lexeme $
      liftM3 (,,) (integer <* colon) (integer <* colon) integer
  let day = fromGregorian (fromIntegral year_) month_ day_
  let timeOfDay = TimeOfDay hour_ minute_ (fromIntegral second_)
  let localTime = LocalTime day timeOfDay
  pure
    Header
      { gameId = handID_,
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
      Fold <$ choice [string "Folds & shows " >> brackets (manyCardsP sc)]
    pCheck :: Parser (BetAction SomeBetSize)
    pCheck =
      Check
        <$ (choice . fmap (string . ("Checks" <>)))
          [" (timeout)", " (disconnect)", ""]
    pCall = Call <$> (string "Calls " *> pAmount)
    pRaise = uncurry Raise <$> (string "Raises " >> pAmountFromTo)
    pBet = Bet <$> (string "Bets " *> pAmount)
    pAllInRaise =
      uncurry AllInRaise <$> (string "All-in(raise) " >> pAmountFromTo)
    pAllIn = AllIn <$> (string "All-in " *> pAmount)
    pAmountFromTo = liftM2 (,) pAmount (string "to " *> pAmount)

-- pDealer matches dealer announcements and exhibits how awful Bovada's format is
pDealer :: Parser ()
pDealer = try pSetDealerPosition <|> symbol_ "Set dealer"
  where
    pSetDealerPosition = do
      string "Dealer " >> optional_ (string " [ME] ")
      void $ pSetDealerTxt *> brackets integer
    pSetDealerTxt = string ": Set dealer " <|> string ": Set deale  r "

pSmallBlind :: Parser (Maybe (TableAction SomeBetSize))
pSmallBlind = label "Small blind post" . optional $ do
  p <- pSmallBlindPosition <* optional_ (string "[ME] ")
  string ": Small Blind " >> mkPost p <$> pAmount
  where
    pSmallBlindPosition = SB <$ symbol "Small Blind" <|> BU <$ symbol "Dealer"
    mkPost p = KnownPlayer p . Post

pBigBlind :: Parser (TableAction SomeBetSize, SomeBetSize)
pBigBlind = label "Big blind post" $ do
  string "Big Blind " >> optional_ (string " [ME] ")
  string_ ": Big blind "
  bb <- pAmount
  pure (mkBBPost bb, bb)
  where
    mkBBPost = KnownPlayer BB . Post

pFlop :: Parser (Action t)
pFlop = do
  string_ "*** FLOP *** "
  [c1, c2, c3] <- brackets (countCard 3 sc)
  pure . MkDealerAction $ FlopDeal c1 c2 c3

turnStreetP :: Parser (Action t)
turnStreetP = do
  string_ "*** TURN *** "
  void $ brackets (countCard 3 sc)
  MkDealerAction . TurnDeal <$> brackets pCard

riverStreetP :: Parser (Action t)
riverStreetP = do
  string_ "*** RIVER *** "
  void $ brackets (countCard 4 sc)
  MkDealerAction . RiverDeal <$> brackets pCard

pHoldingsMap :: Parser (DealerAction, Map Position Hand)
pHoldingsMap = label "Card deal" $ do
  symbol_ "*** HOLE CARDS ***"
  holdingMap <- M.fromList <$> many (try pDeal)
  pure (PlayerDeal, holdingMap)
  where
    pDeal = do
      (p, _) <- pPosition
      string_ ": Card dealt to a spot "
      h <- brackets $ liftM2 unsafeMkHand (lexeme pCard) pCard
      pure (p, h)

pStack :: Parser (Int, Position, SomeBetSize, IsHero)
pStack = do
  seat <- string "Seat " *> decimal <* colon
  (pos, hero) <- pPosition
  stack <- lexeme $ parens (pAmount <* string "in chips")
  pure (seat, pos, stack, hero)

pSummary :: Parser ()
pSummary = string_ "*** SUMMARY ***"

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
      UnknownShowdown <$ maybePositioned_ (string "Showdown(High Card)")
    pSeatStand = SeatStand <$ maybePositioned_ (string "Seat stand")
    pSimpleUnknown =
      try pTableLeaveOrEnter <|> try pSeatUpdate
        <|> choice
          [Enter <$ string "Enter(Auto)", Leave <$ "Leave(Auto)"]

    pReturnUncalled =
      Return <$> (string "Return uncalled portion of bet " >> pAmount)
    pResult =
      Result
        <$> ( choice [string "Hand result-Side pot ", string "Hand result "]
                *> pAmount
            )
    pMuck = do
      showdownStr <-
        choice . fmap string $ ["Mucks ", "Does not show ", "Showdown "]
      cards <- lexeme (brackets (manyCardsP sc))
      (sc <* (lexeme . parens . many) (lexeme letterChar))
        <|> void (many printChar)
      pure $ Showdown cards showdownStr
    pLeave =
      (Leave <$) . choice . fmap string $ ["Leave(Auto)", "Table leave user"]
    pEnter =
      (Enter <$) . choice . fmap string $ ["Enter(Auto)", "Table enter user"]
    pRejoin = Rejoin <$ string "Seat re-join"
    pDeposit = Deposit <$> (string "Table deposit " *> pAmount)
    pSitDown = SitDown <$ string "Seat sit down"
    pSitOut = SitOut <$ string "Seat sit out"

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
      [ PostDead <$> (string "Posts dead chip " >> pAmount),
        PostDead <$> ((string "Posts chip " <|> string "Posts ") *> pAmount)
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
  Map Position Hand ->
  Map Seat (Seat, Position, Player t)
getPlayers stacks holdings =
  let players = M.fromList $ do
        (seat_, pos_, stack_, _) <- stacks
        pure $
          (,)
            (Seat seat_)
            ( Seat seat_,
              pos_,
              Player -- _name           = Just "test"
              -- , _playerPosition = Just pos_
                { _playerHolding = M.lookup pos_ holdings,
                  _stack = stack_
                }
                -- , _seat           = MkSeat seat_
            )
   in players

street :: Parser (Action SomeBetSize) -> Parser [Action SomeBetSize]
street streetHeader = lexeme $ liftM2 (:) streetHeader (many $ try pAction)

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
pTableDeposit = Deposit <$> (string "Table deposit " >> pAmount)

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
