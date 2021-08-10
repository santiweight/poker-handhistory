{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Poker.History.Parse where

import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Void                      ( Void )
import           Poker.History.Parse.Bovada
import           Poker.History.Types
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( (</>) )
import           System.IO.Extra
import           Test.Hspec
import           Test.Tasty.HUnit
import           Text.Megaparsec                ( ParseErrorBundle
                                                , errorBundlePretty
                                                , parse
                                                )
import           Text.Regex.Posix

testHandsPaths :: IO [FilePath]
testHandsPaths = do
  testDir <- (</> "test/histories/small") <$> getCurrentDirectory
  listDirectory testDir <&> fmap (testDir </>)

importedHandsPathsIO :: IO [FilePath]
importedHandsPathsIO = do
  testDir <- (</> "test/histories/Imported Hands") <$> getCurrentDirectory
  listDirectory testDir <&> fmap (testDir </>)

_testFilePath :: FilePath -> IO [History Bovada SomeBetSize]
_testFilePath fp = either (error . errorBundlePretty) id <$> parseFile fp

parseFile
  :: FilePath -> IO (Either (ParseErrorBundle Text Void) [History Bovada SomeBetSize])
parseFile f = do
  file <- T.readFile f
  return . parseString $ file

parseString
  :: Text -> Either (ParseErrorBundle Text Void) [History Bovada SomeBetSize]
parseString = parse pHands []

-- unit_allHands = mapM_ testParseHands =<< testHandsPaths

spec_allHands :: SpecWith ()
spec_allHands = do
  paths <- runIO testHandsPaths
  sequence_ $ paths <&> \p -> do
    it (show p) $ testParseHands p `shouldReturn` ()
  bigFp <- runIO $ getCurrentDirectory <&> (</> "test/histories/bovada-example.txt")
  it (show bigFp) $ testParseHands bigFp `shouldReturn` ()
  importedHandsPaths <- runIO importedHandsPathsIO
  sequence_ $ importedHandsPaths <&> \p -> do
    it (show p) $ testParseHands p `shouldReturn` ()


testParseHands :: FilePath -> IO ()
testParseHands fp = do
  handFileContents <- readFile' fp
  let handTexts :: [MatchArray] = matchAll
        (makeRegex ("\r\n\r\n\r\n" :: String) :: Regex)
        handFileContents
  let numHandsExpected = length handTexts
  let res              = parseString $ T.pack handFileContents
  let hs               = either (error . errorBundlePretty) id res
  assertEqual "expect all hands parsed" (length hs) (numHandsExpected + 1)
