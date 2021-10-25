{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Poker.History.PokerStars.Parser
  ( pHands,
  )
where

import Control.Applicative (liftA2)
import Control.Monad
import Control.Monad.State.Strict
  ( MonadState (get),
    evalStateT,
    gets,
  )
import Data.Function
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( fromJust,
    fromMaybe,
    listToMaybe,
    mapMaybe,
  )
import qualified Data.Set as Set
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime
  ( LocalTime (..),
    TimeOfDay (..),
  )
import Data.Tuple (swap)
import Data.Void (Void)
import Poker
import Poker.History.Base
import Poker.History.Types
import Poker.History.PokerStars.Model
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (id)

type PosParser m = (MonadParsec Void Text m, MonadState (Map Text Position) m)

pManyCards :: MonadParsec Void Text m => m () -> m [Card]
pManyCards pBetween = try pCard `sepEndBy` pBetween <?> "Multiple Cards"

-- TODO make this return a more complex datatype HandHeader
-- TODO accept multiple hand header types
-- TODO match specific network based on header
-- TODO find out if it's yyyy/mm/dd or yyyy/dd/mm
-- TODO figure out time format
-- TODO match different game types eg PLO
handHeaderP ::
  forall m. MonadParsec Void Text m => m (Header, Seat)
handHeaderP = do
  sc
  _handNetwork <- string "PokerStars "
  zoneMay <- optional $ Zoom <$ string "Zoom "
  string_ "Hand #"
  handId <- lexeme integer <?> "Hand Number"
  _ <-
    string ":  Hold'em No Limit "
      *> parens (pAmount *> fwdSlash *> pAmount >> optional (string "USD"))
  _ <- symbol "-"
  let pDate =
        lexeme $
          liftM3 (,,) (integer <* char '/') (integer <* char '/') integer
  let pTime =
        lexeme $ liftM3 (,,) (integer <* colon) (integer <* colon) integer
  let pDateAndTime =
        pDate *> sc *> pTime *> sc *> (string "ET" <|> string "UTC")
  (year_, month_, day_) <- pDate
  (hour_, minute_, second_) <- pTime
  _ <- (string "UTC" <|> string "ET") *> sc *> optional (brackets pDateAndTime)
  let day = fromGregorian (fromIntegral year_) month_ day_
  let timeOfDay = TimeOfDay hour_ minute_ (fromIntegral second_)
  let localTime = LocalTime day timeOfDay
  buttonSeatNum <-
    string "Table "
      *> singleQuotes (many (satisfy (/= '\'')))
      *> decimal @_ @_ @_ @Int
      *> string "-max Seat #"
      *> decimal @_ @_ @_ @Int
      <* string " is the button"
      <* eol
  pure
    ( Header
        { gameId = handId,
          gameTy = fromMaybe Cash zoneMay,
          time = localTime
        },
      Seat buttonSeatNum
    )

pActionValue :: MonadParsec Void Text m => m (BetAction SomeBetSize)
pActionValue =
  choice
    . fmap try
    $ [pFold, pCheck, pBetsAllIn, pAllInRaise, pAllIn, pRaise, pCall, pBet]
  where
    -- TODO if someone folds faceup we get more info!
    pFold =
      Fold
        <$ ( string "folds"
               >> optional (try $ space *> brackets (pManyCards space))
           )
    pCheck = Check <$ string "checks"
    pCall = Call <$> (string "calls " *> pAmount)
    pRaise = uncurry Raise <$> (string "raises " >> pAmountToAmount)
    pBet = Bet <$> (string "bets " *> pAmount)
    pBetsAllIn = AllIn <$> (string "bets " *> pAmount <* string "and is all-in")
    pAllInRaise =
      uncurry AllInRaise
        <$> (string "raises " >> pAmountToAmount <* string "and is all-in")
    pAllIn = AllIn <$> (string "calls " *> pAmount <* string "and is all-in")
    pAmountToAmount = liftM2 (,) pAmount (string "to " *> pAmount)

data BlindPost t = PostSB t | PostBB t | PostDeadBlind t | PostSuperDeadBlind t
  deriving (Show)

pPostBlind :: PosParser m => m (Position, BlindPost SomeBetSize)
pPostBlind = label "Post Blind" . try $ do
  pos <- pPosition <* colon
  smallBlindPoster' <- smallBlindPoster
  post <-
    choice
      -- TODO whether or not these actions are dead posts is context
      -- sensitive (depending on which position posted :( )
      -- Maybe it's right now? Maybe it's not...
      [ (if pos == BB then PostBB else PostDeadBlind) <$ string "posts big blind ",
        (if pos == smallBlindPoster' then PostSB else PostSuperDeadBlind) <$ string "posts small blind ",
        PostDeadBlind <$ string "posts small & big blinds "
      ]
      <*> pAmount

  pure (pos, post)

pFlop :: MonadParsec Void Text m => m (Action t)
pFlop = do
  string_ "*** FLOP *** "
  brackets (countCard 3 sc) <&> \case
    [c1, c2, c3] -> MkDealerAction $ FlopDeal c1 c2 c3
    _ -> error "WTF! I said 3 cards..."

turnStreetP :: MonadParsec Void Text m => m (Action t)
turnStreetP = do
  string_ "*** TURN *** "
  void $ brackets (countCard 3 sc)
  MkDealerAction . TurnDeal <$> brackets pCard

riverStreetP :: MonadParsec Void Text m => m (Action t)
riverStreetP = do
  string_ "*** RIVER *** "
  void $ brackets (countCard 4 sc)
  MkDealerAction . RiverDeal <$> brackets pCard

pHoldingsMap :: PosParser m => m (DealerAction, Map Position Hole)
pHoldingsMap = label "Card deal" $ do
  line_ $ string "*** HOLE CARDS ***"
  holdingMap <- M.fromList <$> many (try pDeal)
  pure (PlayerDeal, holdingMap)
  where
    pDeal =
      liftM2
        (,)
        (string "Dealt to " *> pPosition)
        (brackets $ liftM2 unsafeHole (pCard <* space) pCard)

pStack :: MonadParsec Void Text m => m (Int, String, SomeBetSize)
pStack = try $ do
  seat <- string "Seat " *> decimal <* colon
  userName <- T.unpack . T.strip . T.pack <$> many (notFollowedBy (string "($") *> anySingle)
  stack <- parens (pAmount <* string "in chips")
  pure (seat, userName, stack)

pSummary :: MonadParsec Void Text m => m ()
pSummary = line_ $ string "*** SUMMARY ***"

-- pBoard takes the input 'Board [Cards]' and gives the contained cards
pBoard :: MonadParsec Void Text m => m [Card]
pBoard =
  optional (string "FIRST " <|> string "SECOND ") >> string "Board "
    >> brackets
      (pManyCards sc)

pSaid :: PosParser m => m (Action t)
pSaid = do
  (pos, T.pack -> saidTxt) <-
    (,) <$> (pPosition <* string "said, ")
      <*> many
        (anySingleBut '\n')
  guard
    (T.length saidTxt >= 2 && T.head saidTxt == '"' && T.last saidTxt == '"')
    <?> "quote delimited text"
  pure
    . MkTableAction
    . KnownPlayer pos
    . PlayerSaid
    $ T.drop 1
      . T.dropEnd 1
      $ saidTxt

pLeave :: MonadParsec e Text m => m (Action t)
pLeave =
  (MkTableAction (UnknownPlayer Leave) <$)
    . try
    $ many (notFollowedBy (eol <|> string "leaves the table") *> anySingle)
      >> string "leaves the table"

pJoins :: MonadParsec Void Text m => m (Action t)
pJoins = do
  seatNum <-
    try $
      many (notFollowedBy (eol <|> string "joins the table") *> anySingle)
        >> string "joins the table at seat #"
        *> lexeme integer
  pure $ MkTableAction (UnknownPlayer (Join (Seat seatNum)))

-- pAction parses a simple player action
pAction :: PosParser m => m (Action SomeBetSize)
pAction =
  lexeme $
    try pSaid
      <|> MkTableAction
      <$> pFailToPost
      <|> pLeave
      <|> pJoins
      <|> try (liftA2 MkBetAction (pPosition <* colon) pActionValue)
      <|> try (MkTableAction <$> pTableAction)

pFailToPost :: MonadParsec e Text m => m (TableAction t)
pFailToPost =
  label "Failed to post" $
    (UnknownPlayer FailToPost <$) . try $
      lineEndedBy
        "was removed from the table for failing to post"

pStacks :: MonadParsec Void Text m => m [(Int, String, SomeBetSize)]
pStacks = some pStack

getPlayers ::
  Map Seat Position ->
  Map Seat t ->
  Map Text Position ->
  Map Seat (Position, Player t)
getPlayers seatMap stackMap nameMap =
  let players = M.fromList $ do
        (seat_, _) <- Map.toList seatMap
        let pos = fromJust $ Map.lookup seat_ seatMap
        pure $
          (,)
            seat_
            ( pos,
              Player
                { _name =
                    fromJust $
                      Map.lookup
                        pos
                        (Map.fromList . fmap swap . Map.toList $ nameMap),
                  _stack = fromJust $ Map.lookup seat_ stackMap
                }
            )
   in players

street :: PosParser m => m (Action SomeBetSize) -> m [Action SomeBetSize]
street streetHeader = lexeme $ liftM2 (:) streetHeader (many $ try pAction)

streets :: PosParser m => m [Action SomeBetSize]
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

pStreetHeader ::
  ( MonadParsec e s m,
    Semigroup (Tokens s),
    IsString (Tokens s)
  ) =>
  Tokens s ->
  m (Tokens s)
pStreetHeader streetName = string $ "*** " <> streetName <> " ***"

-- TODO this is inefficient - consume the line once only
pTableAction ::
  forall e t m.
  (MonadState (Map Text Position) m, MonadParsec e Text m) =>
  m (TableAction t)
pTableAction = do
  (lineTxt, res) <- lineThat tryMatchSuffix
  validUsers <- Map.assocs <$> get
  case listToMaybe $
    mapMaybe
      (\(playerName, pos) -> pos <$ T.stripPrefix playerName lineTxt)
      validUsers of
    Nothing -> pure $ UnknownPlayer res
    Just pos -> pure $ KnownPlayer pos res
  where
    lineThat :: (MonadParsec e Text m) => (Text -> Maybe a) -> m (Text, a)
    lineThat f = do
      (T.pack -> lineTxt) <- many (notFollowedBy eol *> anySingle)
      case f lineTxt of
        Nothing -> empty
        Just a -> (lineTxt, a) <$ eol_
    tryMatchSuffix lineTxt =
      ignorableActs
        & listToMaybe
          . mapMaybe
            ( \(act, matcher) ->
                if matcher `T.isSuffixOf` lineTxt then Just act else Nothing
            )
    ignorableActs =
      [ (SittingOut, "is sitting out "),
        (SittingOut, "is sitting out"),
        (AllowedToPlayerAfterButton, "will be allowed to play after the button"),
        (TimeOut, "has timed out"),
        (TimeOutWhileDisconnected, "has timed out while disconnected"),
        (TimeOutWhileDisconnected, "has timed out while being disconnected"),
        (TimeOut, "has timed out"),
        (IsDisconnected, "is disconnected"),
        (IsDisconnected, "is disconnected "),
        (IsConnected, "is connected"),
        (IsConnected, "is connected "),
        (Leave, "leaves the table"),
        (SitOut, "sits out"),
        (SitOut, "sits out ")
      ]

pShows :: MonadParsec Void Text m => m String
pShows =
  string "shows " >> brackets (pManyCards sc)
    >> parens
      (many $ satisfy (`notElem` ("\n)" :: String)))

-- handP is the primary hand parser that matches a hand
pHand :: Parser (History SomeBetSize) -- Hand
pHand = do
  lineNum <- getParserState <&> sourceLine . pstateSourcePos . statePosState
  (header', btnSeatNum) <- handHeaderP
  stacks <- pStacks
  let seatToStack = Map.fromList $ do
        (seat, _, stack) <- stacks
        pure (Seat seat, stack)
  let nameToSeat =
        Map.fromList $ stacks <&> (\(seat, name, _) -> (T.pack name, Seat seat))
  let allSeats = Set.toAscList $ Map.keysSet seatToStack
  let seatToPos :: Map Seat Position =
        Map.fromList $
          zip
            (take (length allSeats) . dropWhile (/= btnSeatNum) $ cycle allSeats)
            (dropWhile (/= BU) $ cycle (if length allSeats == 2 then [BU, BB] else allPossiblePositions))
  let nameMap :: Map Text Position =
        nameToSeat
          & Map.mapWithKey
            ( \_ name ->
                fromMaybe
                  ( error $
                      TL.unpack
                        . TL.intercalate "\n"
                        $ TL.pack <$> [show lineNum, show nameToSeat, show seatToPos]
                  )
                  . flip Map.lookup seatToPos
                  $ name
            )
  flip evalStateT nameMap $ do
    postAs <-
      many $
        try (many (try_ pTableAction <|> void pAction) *> pPostBlind)
    let bbPostAsMay =
          postAs <&> \(_, bp) -> case bp of
            (PostBB bb) -> Just bb
            _ -> Nothing
    let bbs =
          fromMaybe (error $ "Someone needs to post a big blind " <> show lineNum) $
            choice bbPostAsMay
    -- TODO careful not to dismiss valid acts
    _ <- many $ try pAction
    -- TODO keep antes
    antes <- do
      many . try $ liftA2 (\pos -> KnownPlayer pos . Ante) (pPosition <* colon) (string "posts the ante " *> pAmount)
    _ <-
      many
        . choice
        . fmap try
        $ [lineEndedBy "sits out ", lineEndedBy "sits out"]
    (preFlopDeal, _) <- pHoldingsMap
    postFlopAs <- streets
    _ <- many (try pWinnerResult <* many (try pAction))
    optional_ (pFlop >> optional (turnStreetP >> optional riverStreetP))
    optional_ (turnStreetP >> optional riverStreetP)
    optional_ riverStreetP
    optional_ . line_ $ pStreetHeader "FIRST FLOP" *> many (anySingleBut '\n')
    optional_ . line_ $ pStreetHeader "FIRST TURN" *> many (anySingleBut '\n')
    optional_ . line_ $ pStreetHeader "FIRST RIVER" *> many (anySingleBut '\n')
    _ <- many $ try pAction
    optional_ . line_ $ pStreetHeader "SECOND FLOP" *> many (anySingleBut '\n')
    optional_ . line_ $ pStreetHeader "SECOND TURN" *> many (anySingleBut '\n')
    optional_ . line_ $ pStreetHeader "SECOND RIVER" *> many (anySingleBut '\n')
    let pShowDown num = optional $ do
          line_ $ string ("*** " <> num <> "SHOW DOWN ***")
          let pMucks = symbol "mucks hand"
          _ <- many . label "Showdown act" $ do
            choice
              . fmap try
              $ [ pPosition >> colon >> (void pShows <|> void pMucks),
                  void pFailToPost,
                  void $ lexeme pLeave,
                  void $
                    pPosition
                      >> string "collected "
                      >> pAmount
                      >> symbol "from "
                      >> choice
                        ( try (string "side pot-" <* lexeme integer) :
                          (symbol <$> ["main pot", "side pot", "pot"])
                        ),
                  void pTableAction,
                  void pJoins
                ]
          many . label "Cash out" $ do
            pPosition >> string "cashed out the hand for " >> pAmount
              >> optional
                (string "| Cash Out Fee " >> pAmount)
    optional_ (pShowDown "")
    optional_
      (try $ pShowDown "FIRST " >> many (try pAction) >> pShowDown "SECOND ")
    pSummary
    pPotResult
    optional_ $ line_ $ string "Hand was run twice"
    _ <- many $ try pBoard
    _ <- many pPlayerResult
    -- -- TODO parse summary
    -- _ <- many (try $ notFollowedBy (lookAhead (eol >> eol)) >> anySingle)
    let getPostSize :: BlindPost SomeBetSize -> TableActionValue SomeBetSize
        getPostSize (PostSB sbs) = Post sbs
        getPostSize (PostBB sbs) = Post sbs
        getPostSize (PostDeadBlind sbs) = PostDead sbs
        getPostSize (PostSuperDeadBlind sbs) = PostSuperDead sbs
    -- TODO some of these posts are dead, not real posts??
    let allAs =
          concat
            [ (\(po, bp) -> MkTableAction (KnownPlayer po $ getPostSize bp))
                <$> postAs,
              MkTableAction <$> antes,
              [MkDealerAction preFlopDeal],
              postFlopAs
            ]
    let players = getPlayers seatToPos seatToStack nameMap
    pure
      $! History
        { header = header',
          _handStakes = Stake bbs,
          _handActions = allAs,
          _handPlayerMap = players <&> snd,
          _handSeatMap = seatToPos,
          _handText = ""
        }

pPosition ::
  (MonadState (Map Text Position) m, MonadParsec Void Text m) => m Position
pPosition = getPos =<< choice . fmap symbol . Map.keys =<< get
  where
    getPos = gets . fmap fromJust . Map.lookup . T.stripEnd

smallBlindPoster ::
  (MonadState (Map Text Position) m, MonadParsec Void Text m) => m Position
smallBlindPoster = Map.size <$> get <&> \case
                          2 -> BU
                          _ -> SB

pPlayerResult :: PosParser m => m ()
pPlayerResult =
  choice
    [ void $
        string "Seat "
          >> decimal @_ @_ @_ @Int
          >> colon
          >> pPosition
          >> many
            ( try $
                parens
                  (choice . fmap string $ ["button", "small blind", "big blind"])
            )
          >> pPlayerAct,
      void pFailToPost,
      void $ lexeme pLeave
    ]
  where
    pStreetWord = choice . fmap symbol $ ["Flop", "Turn", "River"]
    pPlayerAct =
      choice
        [ void $
            string "folded before " >> pStreetWord
              >> optional
                (try $ parens (string "didn't bet")),
          void $
            string "showed " >> brackets (pManyCards sc)
              >> choice
                [ line_ $
                    string "and won " >> parens pAmount >> string "with "
                      >> many
                        (anySingleBut '\n'),
                  line_ $ string "and lost with " >> many (anySingleBut '\n')
                ],
          void $
            string "folded on the " >> pStreetWord
              >> optional
                (parens $ string "didn't bet"),
          void $ string "collected " >> parens pAmount,
          void $ string "mucked " >> brackets (pManyCards . void $ char ' '),
          void $ symbol "mucked"
        ]

pPotResult :: MonadParsec Void Text m => m ()
pPotResult =
  void $
    string "Total pot "
      >> pAmount
      >> optional
        ( string "Main pot " >> pAmount >> symbol "."
            >> many
              ( (try (string "Side pot-" <* lexeme integer) <|> string "Side pot ")
                  >> pAmount
                  >> symbol "."
              )
        )
      >> string "| Rake "
      >> pAmount

-- TODO might not need choice, the following actions appear to be ordered
pWinnerResult :: PosParser m => m ()
pWinnerResult =
  void
    . choice
    . fmap try
    $ [ void $
          string "Uncalled bet "
            >> parens pAmount
            >> string "returned to "
            >> pPosition,
        -- NOT GOOD FUCK YOU POKERSTARS - PUT YOUR USENAMES IN QUOTES!
        void pFailToPost,
        lexeme $ void pLeave,
        line_ $
          lexeme pPosition
            >> string "collected "
            >> pAmount
            >> (string "from pot" <|> try (string "from side pot-" <* lexeme integer)),
        void $ pPosition *> colon *> pShows,
        line_ $
          pPosition
            >> colon
            >> (string "doesn't show hand " <|> string "doesn't show hand"),
        line_ pSaid,
        line_ $ pPosition >> string "is disconnected ",
        pPosition >> string ":  " >> eol_,
        pPosition >> string ":" >> eol_
      ]

-- handsP is the the highest level parser
pHands :: Parser [History SomeBetSize]
pHands = between sc eof $ do
  res <- many (lexeme pHand)
  _ <- many eol
  pure res
