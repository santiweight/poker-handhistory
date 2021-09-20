module Test.Poker.History.Bovada.Hands where

import           Data.Functor
import           Money
import           Paths_poker_handhistory
import           Poker
import           Poker.History.Parse.Bovada     ( pHands )
import           Poker.History.Types
import           System.Directory               ( listDirectory )
import           System.FilePath
import           Text.Megaparsec
import qualified Data.Text.IO as T
import Control.Monad (forM)

historyFilePathsIO :: IO [FilePath]
historyFilePathsIO = do
  testDir <- getDataDir <&> (</> "Bovada")
  listDirectory testDir <&> fmap (testDir </>)

allHands :: IO [History Bovada (Amount "USD")]
allHands = do
  historyFilePaths <- historyFilePathsIO
  fmap concat . forM historyFilePaths $ \historyFp -> do
    historyFile <- T.readFile historyFp
    case parse pHands historyFp historyFile of
      Left peb -> error $ errorBundlePretty peb
      Right hiss -> pure . fmap (fmap toUsd) $ hiss

toUsd :: SomeBetSize -> Amount "USD"
toUsd (SomeBetSize USD ra) = Amount . fst . discreteFromDense Floor $ dense' ra
toUsd _ = error "Unexpected non-USD hand"
