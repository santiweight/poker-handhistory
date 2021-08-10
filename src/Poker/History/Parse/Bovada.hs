{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Poker.History.Parse.Bovada where

import           Control.Monad
import           Data.Foldable                  ( Foldable(toList) )
import           Data.Functor
import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.LocalTime            ( LocalTime(..)
                                                , TimeOfDay(..)
                                                )
import           Poker.Base
import           Poker.History.Parse.Base
import           Poker.History.Types
import           Prelude                 hiding ( id )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer     ( decimal )

-- currecny matches a currency symbol
-- TODO make currency a Parser Currency
currency :: Parser SomeCurr
currency = anySingle >>= \case
  '$' -> pure $ SomeCurr USD
  '€' -> pure $ SomeCurr EUR
  '£' -> pure $ SomeCurr GBP
  chr -> fail $ "Unexpected currency '" <> [chr] <> "'"
-- oneOf ("$€£" :: String) <?> "currency symbol"

pCard :: Parser Card
pCard = parsePrettyP

-- | Multiple cards
manyCardsP :: Parser [Card]
manyCardsP = try pCard `sepEndBy` (sc <|> string_ ",") <?> "Multiple Cards"

-- | Match a specified number of cards
countCard :: Int -> Parser [Card]
countCard num = do
  cards <- try pCard `sepEndBy` (try sc <|> string_ ",") <?> "Multiple Cards"
  if length cards /= num
    then
      fail
      $  "Unexpected number of cards: Expected "
      <> show num
      <> " but found "
      <> show (length cards)
    else pure cards

-- TODO change this to pCurrencyAmount
-- TODO parse currency type
amountP :: Parser SomeBetSize
amountP = lexeme
  (currency >>= \case
    SomeCurr curr -> SomeBetSize curr <$> rational
  )

-- parses a Position type
pPosition :: Parser (Position, IsHero)
pPosition = do
  p    <- lexeme positionString
  hero <- option Villain $ lexeme (string "[ME]") $> Hero
  return (p, hero)
 where
  positionString =
    choice
        [ SB <$ string "Small Blind"
        , BB <$ string "Big Blind"
        , UTG1 <$ string "UTG+1"
        , UTG2 <$ string "UTG+2"
        , UTG <$ string "UTG"
        , BU <$ string "Dealer"
        ]
      <?> "expected Position"

-- TODO make this return a more complex datatype HandHeader
-- TODO accept multiple hand header types
-- TODO match specific network based on header
-- TODO find out if it's yyyy/mm/dd or yyyy/dd/mm
-- TODO figure out time format
-- TODO match different game types eg PLO
handHeaderP :: Parser (Header Bovada)
handHeaderP = do
  _handNetwork <-
    lexeme
        (choice [Bovada <$ string "Bovada", PokerStars <$ string "PokerStars"])
      <?> "Network"
  string_ "Hand #"
  handID_ <- integer <?> "Hand Number"
  zoneMay <- lexeme . optional $ Zone <$ string "Zone Poker"
  _       <- lexeme . optional $ string "ID#" *> integer
  _       <- lexeme . optional $ string "TBL#" *> integer
  lexeme_ $ string "HOLDEM" *> optional (string "ZonePoker") <* string
    " No Limit -"
  (year_, month_, day_) <- lexeme
    $ liftM3 (,,) (integer <* char '-') (integer <* char '-') integer
  (hour_, minute_, second_) <- lexeme
    $ liftM3 (,,) (integer <* char ':') (integer <* char ':') integer
  let day       = fromGregorian (fromIntegral year_) month_ day_
  let timeOfDay = TimeOfDay hour_ minute_ (fromIntegral second_)
  let localTime = LocalTime day timeOfDay
  return Header { gameId       = handID_
                , gameTy   = fromMaybe Cash zoneMay
                , sNetwork = SBovada -- TODO
                , time     = localTime
                }

pActionValue :: Parser (BetAction SomeBetSize)
pActionValue =
  choice
    . fmap try
    $ [ pComplexFold
      , pFold
      , pCheck
      , pCall
      , pRaise
      , pBet
      , pAllInRaise
      , pAllIn
      , pOtherAction
      ]
 where
  pFold = Fold <$ (choice . fmap string)
    ["Folds (timeout)", "Folds (disconnect)", "Folds (auth)", "Folds"]
  pComplexFold =
    Fold <$ choice [string "Folds & shows " >> inBrackets manyCardsP]
  pCheck :: Parser (BetAction SomeBetSize)
  pCheck = Check <$ (choice . fmap string)
    ["Checks (timeout)", "Checks (disconnect)", "Checks"]
  pCall  = Call <$> (string "Calls " *> amountP)
  pRaise = uncurry Raise <$> (string "Raises " >> amountPFromTo)
  pBet   = Bet <$> (string "Bets " >> amountP)
  pAllInRaise =
    uncurry AllInRaise <$> (string "All-in(raise) " >> amountPFromTo)
  pAllIn = AllIn <$> (string "All-in " >> amountP)
  pOtherAction =
    OtherAction <$ (string "Seat stand" <|> string "Showdown(High Card)")
  amountPFromTo =
    liftM2 (,) amountP (string "to " >> amountP) :: Parser
        (SomeBetSize, SomeBetSize)

-- pDealer matches dealer announcements and exhibits how awful Bovada's format is
pDealer :: Parser ()
pDealer = try . lexeme_ $ do
  pSetDealerPosition <|> lexeme (string_ "Set dealer")
 where
  pSetDealerPosition = do
    string "Dealer " >> optional_ (string " [ME] ")
    void $ pSetDealerTxt >> inBrackets integer
  pSetDealerTxt = string ": Set dealer " <|> string ": Set deale  r "

pSmallBlind :: Parser (Maybe (TableAction SomeBetSize))
pSmallBlind = label "Small blind post" . optional $ do
  p <- lexeme pSmallBlindPosition <* optional_ (string "[ME] ")
  string ": Small Blind " >> mkPost p <$> amountP

 where
  pSmallBlindPosition = SB <$ string "Small Blind" <|> BU <$ string "Dealer"
  mkPost p = TableAction p . Post

pBigBlind :: Parser (TableAction SomeBetSize, SomeBetSize)
pBigBlind = label "Big blind post" $ do
  string "Big Blind " >> optional_ (string " [ME] ")
  string_ ": Big blind "
  bb <- amountP
  return (mkBBPost bb, bb)
  where mkBBPost = TableAction BB . Post

pFlop :: Parser (Action t)
pFlop = do
  pFlopHeading
  [c1, c2, c3] <- lexeme $ inBrackets (countCard 3)
  pure . MkDealerAction $ FlopDeal c1 c2 c3
  where pFlopHeading = string_ "*** FLOP *** "

pTurnHeading :: Parser ()
pTurnHeading = lexeme $ string_ "*** TURN *** "

turnStreetP :: Parser (Action t)
turnStreetP = do
  lexeme pTurnHeading >> lexeme_ (inBrackets manyCardsP)
  MkDealerAction . TurnDeal <$> lexeme (inBrackets pCard)

pRiverHeading :: Parser ()
pRiverHeading = string_ "*** RIVER *** " <?> "River deal"

riverStreetP :: Parser (Action t) --Action
riverStreetP = do
  pRiverHeading
  void $ inBrackets manyCardsP
  dealtcard <- spaceWrap (inBrackets pCard)
  void eol
  return . MkDealerAction $ RiverDeal dealtcard

pHoldingsMap :: Parser (DealerAction, Map Position Hand)
pHoldingsMap = label "Card deal" $ do
  holeCardsHeadingP
  holdingMap <- M.fromList <$> many (try $ lexeme pDeal)
  return (PlayerDeal, holdingMap)
 where
  pDeal = do
    (p, _) <- pPosition
    string_ ": Card dealt to a spot "
    h <- inBrackets $ liftM2 unsafeMkHand (lexeme pCard) pCard
    pure (p, h)
  holeCardsHeadingP = lexeme_ $ string "*** HOLE CARDS ***"

pHolding :: Parser (Position, Hand, IsHero)
pHolding = lexeme $ do
  (pos', isHero') <- pPosition
  spaceWrap colon *> string_ "Card dealt to a spot "
  [c1, c2] <- inBrackets manyCardsP
  return (pos', fromJust $ mkHand c1 c2, isHero')

pStack :: Parser (Int, Position, SomeBetSize, IsHero)
pStack = do
  seat'           <- string "Seat " *> decimal <* colon
  (pos', isHero') <- pPosition
  stack'          <- lexeme $ inParens (amountP <* string "in chips")
  return (seat', pos', stack', isHero')

pSummary :: Parser ()
pSummary = string_ "*** SUMMARY ***"

potP :: Parser SomeBetSize
potP = string "Total Pot" *> inParens amountP

-- pSeatSumm takes
pSeatSumm :: Parser (TableAction t) -- TableAction
pSeatSumm = do
  string "Seat+" >> integer >> string_ ": "
  pPosition >> many printChar $> UnknownAction

-- pBoard takes the input 'Board [Cards]' and gives the contained cards
pBoard :: Parser [Card]
pBoard = string "Board " >> inBrackets manyCardsP

pTableAction :: Parser (TableAction SomeBetSize)-- TableAction
pTableAction =
  lexeme
    $   try
    $   do
          (pos, _)       <- pPosition <* colon
          tableActionVal <- choice
            [ pDeposit
            , pSitDown
            , pSitOut
            , pMuck
            , pResult
            , pLeave
            , pRejoin
            , pEnter
            , pReturnUncalled
            , Deposit <$> pTableDeposit
            ]
          return $ TableAction pos tableActionVal
    <|> try pSimpleUnknown
    <|> try (UnknownAction <$ pTableDeposit)
 where
  pSimpleUnknown =
    (UnknownAction <$ pTableLeaveOrEnter)
      <|> (UnknownAction <$ pSeatUpdate)
      <|> choice
            (fmap ((<$) UnknownAction . string) ["Enter(Auto)", "Leave(Auto)"])
  pReturnUncalled =
    Return <$> (string "Return uncalled portion of bet " >> amountP)
  pResult =
    Result
      <$> (  choice [string "Hand result-Side pot ", string "Hand result "]
          >> amountP
          )
  pMuck = do
    -- TODO make this not shit
    showdownStr :: Text <-
      string "Mucks " <|> string "Does not show " <|> string "Showdown "
    cards <- lexeme (inBrackets manyCardsP)
    (sc <* (lexeme . inParens . many) (lexeme letterChar))
      <|> void (many printChar)
    return $ Showdown cards showdownStr
  pLeave =
    (<$) Leave . choice . fmap string $ ["Leave(Auto)", "Table leave user"]
  pEnter =
    (<$) Enter . choice . fmap string $ ["Enter(Auto)", "Table enter user"]
  pRejoin  = Rejoin <$ string "Seat re-join"
  pDeposit = Deposit <$> (string "Table deposit " *> amountP)
  pSitDown = SitDown <$ string "Seat sit down"
  pSitOut  = SitOut <$ string "Seat sit out"

pTableLeaveOrEnter :: Parser ()
pTableLeaveOrEnter =
  lexeme_ . choice . fmap string $ ["Table leave user", "Table enter user"]

pPost :: Parser (TableAction SomeBetSize)
pPost = do
  (p, _)      <- lexeme pPosition <* lexeme colon
  tableActVal <- choice
    [ PostDead <$> (string "Posts dead chip " >> amountP)
    , Post <$> ((string "Posts chip " <|> string "Posts ") *> amountP)
    ]
  pure $ TableAction p tableActVal

-- pAction parses a simple player action
pAction :: Parser (Action SomeBetSize)
pAction =
  lexeme
    $   try
          (do
            (pos, isHeroVal) <- pPosition <* colon
            actionVal        <- pActionValue
            return . MkPlayerAction $ PlayerAction pos actionVal isHeroVal
          )
    <|> MkTableAction
    <$> try pTableAction

pStacks :: Parser [(Int, Position, SomeBetSize, IsHero)]
pStacks = some pStack

getPlayers
  :: [(Int, Position, t, d)] -> Map Position Hand -> Map Seat (Player t)
getPlayers stacks holdings =
  let players = M.fromList $ do
        (seat_, pos_, stack_, _) <- stacks
        return $ (,)
          (MkSeat seat_)
          Player { _name           = Just "test"
                 , _playerPosition = Just pos_
                 , _playerHolding  = M.lookup pos_ holdings
                 , _stack          = stack_
                 , _seat           = MkSeat seat_
                 }
  in  players

street :: Parser (Action SomeBetSize) -> Parser [Action SomeBetSize]
street streetHeader = lexeme $ liftM2 (:) streetHeader (many $ try pAction)


getSeatMap :: Map k (Player t) -> Map Position Seat
getSeatMap players =
  let func player seatMap' = case _playerPosition player of
        Nothing  -> seatMap'
        Just pos -> M.insert pos (_seat player) seatMap'
  in  M.foldr func Map.empty players

streets :: Parser [Action SomeBetSize]
streets = do
  preflopAs  <- some pAction <?> "Preflop"
  postFlopAs <- option [] $ do
    flopAs         <- street pFlop <?> "Flop"
    turnAndRiverAs <- option [] $ do
      turnAs  <- street turnStreetP <?> "Turn"
      riverAs <- option [] (street riverStreetP <?> "River")
      pure $ turnAs <> riverAs
    pure $ flopAs <> turnAndRiverAs
  pure $ preflopAs <> postFlopAs

pSeatUpdate :: Parser ()
pSeatUpdate =
  lexeme_
    . choice
    . fmap string
    $ ["Seat re-join", "Seat sit out", "Seat sit down", "Seat stand"]

pTableDeposit :: Parser SomeBetSize
pTableDeposit = string "Table deposit " >> amountP

pTableOrSeatLines :: Parser ()
pTableOrSeatLines = do
  void . many $ choice
    [ try . void $ maybePositioned pTableDeposit
    , try $ maybePositioned pSeatUpdate
    , try $ maybePositioned pTableLeaveOrEnter
    ]

maybePositioned :: Parser b -> Parser b
maybePositioned = (optional (pPosition >> colon) >>)

-- handP is the primary hand parser that matches a hand
pHand :: Parser (History Bovada SomeBetSize) -- Hand
pHand = do
  handHeader <- handHeaderP
  stacks     <- pStacks
  optional_ pDealer
  pTableOrSeatLines
  sbActMay <- pSmallBlind
  pTableOrSeatLines
  (bbAct, bb) <- pBigBlind
  pTableOrSeatLines
  postAs                  <- many . try $ pPost
  (preFlopDeal, holdings) <- pHoldingsMap
  postFlopAs              <- streets
  pSummary
  -- TODO parse summary
  _ <- many (try $ notFollowedBy (lookAhead (eol >> eol)) >> anySingle)
  let allAs = concat
        [ toList $ MkTableAction <$> sbActMay
        , [MkTableAction bbAct]
        , MkTableAction <$> postAs
        , [MkDealerAction preFlopDeal]
        , postFlopAs
        ]
  let players = getPlayers stacks holdings
  pure $! History { header         = handHeader
                  , _handStakes    = Stake bb
                  , _handActions   = allAs
                  , _handPlayerMap = players
                  , _handSeatMap   = getSeatMap players
                  , _handText      = ""
                  }

-- handsP is the the highest level parser
pHands :: Parser [History Bovada SomeBetSize]
pHands = between sc eof $ do
  res <- many (lexeme pHand)
  _   <- many eol
  return res

-- -- parseFiles pulls out all the hands from the hands in a list of files
-- -- TODO make everything concurrent and start testing
-- parseFiles :: [FilePath] -> IO [History SomeBetSize]
-- parseFiles = fmap concat . mapConcurrently parseFileUnsafe

-- parseFileUnsafe :: FilePath -> IO [History SomeBetSize]
-- parseFileUnsafe f = do
--   file <- readFile f
--   return . runError . parseString $ file
--   where
--     runError = \case
--       Right b -> b
--       Left e -> error $ errorBundlePretty e

-- parseFile ::
--   FilePath ->
--   IO (Either (ParseErrorBundle String Void) [History SomeBetSize])
-- parseFile f = do
--   file <- readFile f
--   return . parseString $ file

-- -- parse hands in a directory
-- -- drop the first two file names '.' and '..'
-- parseDirectory :: FilePath -> IO [History SomeBetSize]
-- parseDirectory dirName = do
--   filepaths <- traverseDir (const True) transition [] dirName
--   parseFiles filepaths
--   where
--     transition acc new = if ".txt" `isSuffixOf` new then pure (new : acc) else pure acc

-- parseInPath :: FilePath -> IO [History SomeBetSize]
-- parseInPath fp = do
--   isDir <- doesDirectoryExist fp
--   if isDir
--     then parseDirectory fp
--     else parseFile fp >>= \case
--             Left _ -> pure []
--             Right r -> pure r


-- traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
-- traverseDir validDir transition =
--   let go state dirPath =
--         do
--           names <- listDirectory dirPath
--           let paths = map (dirPath </>) names
--           (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
--           state' <- foldM transition state filePaths -- process current dir
--           foldM go state' (filter validDir dirPaths) -- process subdirs
--   in go
--   where
--     partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
--     partitionM _ [] = pure ([], [])
--     partitionM f (x:xs) = do
--         res <- f x
--         (as,bs) <- partitionM f xs
--         pure ([x | res]++as, [x | not res]++bs)
