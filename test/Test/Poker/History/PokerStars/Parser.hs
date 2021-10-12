{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Test.Poker.History.PokerStars.Parser where

import Control.Monad
  ( forM_,
  )
import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.IORef.Extra
  ( IORef,
    newIORef,
    readIORef,
  )
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Poker.History.PokerStars.Model
  ( History,
  )
import Poker.History.PokerStars.Parser
import Poker.History.Types
import System.Directory
  ( getCurrentDirectory,
    listDirectory,
  )
import System.FilePath ((</>))
import Test.Hspec
import Text.Megaparsec
  ( ParseErrorBundle,
    errorBundlePretty,
    parse,
  )

testHandsPaths :: IO [FilePath]
testHandsPaths = do
  testDir <-
    (</> "test/histories/PokerStars/2018/12-29") <$> getCurrentDirectory
  listDirectory testDir <&> fmap (testDir </>)

testHandsPaths2 :: IO [FilePath]
testHandsPaths2 = do
  testDir <-
    (</> "test/example-handhistories/2021-07-30_CO_NL50_FR_OOFGJJQ17")
      <$> getCurrentDirectory
  listDirectory testDir <&> fmap (testDir </>)

parseString ::
  Text ->
  Either (ParseErrorBundle Text Void) [History SomeBetSize]
parseString = parse pHands []

-- unit_allHands = mapM_ testParseHands =<< testHandsPaths

runWithHands :: (Num a1, Show a1) => (IORef a1 -> IO a2) -> IO ()
runWithHands withHands = do
  totalHands <- newIORef 0
  _ <- withHands totalHands
  print =<< readIORef totalHands

newtype NonEq a = NonEq {unNonEq :: a}

instance Eq (NonEq a) where
  _ == _ = False

spec_allHands :: SpecWith ()
spec_allHands = do
  allTestHandsPaths <- runIO allTestHandsPathsIO
  describe "foo" $ do
    forM_ allTestHandsPaths $ \testHandsPaths' -> do
      do
        let foo =
              sequence_ $
                testHandsPaths' <&> \p -> do
                  it (show p) $ do
                    numHands <- testParseHands p
                    numHands `shouldSatisfy` (>= 1)
        foo

        pure ()
  where
    -- allTestHandsPaths = [testHandsPaths]
    allTestHandsPathsIO = sequence [testHandsPaths2]

-- bigFp <- runIO $ getCurrentDirectory <&> (</> "test/histories/bovada-example.txt")
-- it (show bigFp) $ testParseHands bigFp `shouldReturn` ()
-- importedHandsPaths <- runIO importedHandsPathsIO
-- sequence_ $ importedHandsPaths <&> \p -> do
--   it (show p) $ testParseHands p `shouldReturn` ()

dropBOM :: BS.ByteString -> BS.ByteString
dropBOM bs
  | BS.take 3 bs == BS.pack [0xEF, 0xBB, 0xBF] = BS.drop 3 bs
  | otherwise = bs

testParseHands :: FilePath -> IO Int
testParseHands fp = do
  -- getLocaleEncoding >>= print
  handFileContents <- BS.readFile fp
  -- let numHandsExpected = matchAllText
  --       (makeRegex ("(\r\n|\n)(\r\n|\n)(\r\n|\n)" :: ByteString) :: Regex)
  --       (handFileContents)
  let handFileContents' = dropBOM handFileContents
  let res = parseString $ T.decodeUtf8With T.lenientDecode handFileContents'
  let hs = either (error . errorBundlePretty) id res
  pure $ length hs
