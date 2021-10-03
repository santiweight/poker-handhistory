{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Poker.History.Bovada.Parser where

import           Data.Functor                   ( (<&>) )
import qualified Data.Text                     as T
import           Poker.History.Bovada.Parser
import           System.IO.Extra
import           Test.Hspec
import           Test.Poker.History.Bovada.Hands
import           Test.Tasty.HUnit
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )
import           Text.Regex.Posix

spec_allHands :: SpecWith ()
spec_allHands = do
  importedHandsPaths <- runIO historyFilePathsIO
  sequence_ $ importedHandsPaths <&> \p -> do
    it (show p) $ testParseHands p `shouldReturn` ()

testParseHands :: FilePath -> IO ()
testParseHands fp = do
  handFileContents <- readFile' fp
  let handTexts :: [MatchArray] = matchAll
        (makeRegex ("\r\n\r\n\r\n" :: String) :: Regex)
        handFileContents
  let numHandsExpected = length handTexts
  let res              = parse pHands fp $ T.pack handFileContents
  let hs               = either (error . errorBundlePretty) id res
  assertEqual "expect all hands parsed" (length hs) (numHandsExpected + 1)
