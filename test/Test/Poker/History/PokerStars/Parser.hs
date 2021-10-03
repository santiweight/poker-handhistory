{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE BangPatterns #-}

module Test.Poker.History.PokerStars.Parser where

import           Control.Exception              ( SomeException(SomeException)
                                                , catch
                                                , try
                                                )
import           Control.Monad                  ( forM_
                                                , void, (<=<)
                                                )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Either                    ( fromRight )
import           Data.Either.Extra              ( eitherToMaybe )
import           Data.Functor                   ( (<&>) )
import           Data.IORef.Extra               ( modifyIORef
                                                , newIORef
                                                , readIORef
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as BS
import qualified Data.Text.Encoding            as T
import           Data.Text.Encoding.Error       ( UnicodeException )
import qualified Data.Text.Encoding.Error      as T
import qualified Data.Text.IO                  as T
import           Data.Void                      ( Void )
import           GHC.IO                         ( catchException )
import           GHC.IO.Encoding                ( getLocaleEncoding )
import           Poker.History.PokerStars.Parser
import           Poker.History.PokerStars.Model            ( History
                                                , Network(PokerStars)
                                                , SomeBetSize
                                                )
import           System.Directory               ( getCurrentDirectory
                                                , listDirectory
                                                )
import           System.FilePath                ( (</>) )
import           System.IO.Extra
import           Test.Hspec
import           Test.Tasty.HUnit
import           Text.Megaparsec                ( (<|>)
                                                , ParseErrorBundle
                                                , errorBundlePretty
                                                , parse
                                                )
import           Text.Regex.TDFA                ( Regex
                                                , RegexLike(matchAllText)
                                                , RegexMaker(makeRegex)
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

_testFilePath :: FilePath -> IO [History PokerStars SomeBetSize]
_testFilePath fp = either (error . errorBundlePretty) id <$> parseFile fp

parseFile
  :: FilePath
  -> IO
       (Either (ParseErrorBundle Text Void) [History PokerStars SomeBetSize])
parseFile f = do
  file <- T.readFile f
  return . parseString $ file

parseString
  :: Text
  -> Either (ParseErrorBundle Text Void) [History PokerStars SomeBetSize]
parseString = parse pHands []

-- unit_allHands = mapM_ testParseHands =<< testHandsPaths

runWithHands withHands = do
    totalHands <- newIORef 0
    withHands totalHands
    print =<< readIORef totalHands

data NonEq a = NonEq { unNonEq :: a}

instance Eq (NonEq a) where
  ne == ne' = False

spec_allHands :: SpecWith ()
spec_allHands =   do
  allTestHandsPaths <- runIO allTestHandsPathsIO
  describe "foo" $ do
    forM_ allTestHandsPaths $ \testHandsPaths' -> do
      do
        -- around (\withHands -> do
        --   totalHands <- newIORef 0
        --   withHands totalHands
        --   print =<< readIORef totalHands)
        let foo = sequence_ $ testHandsPaths' <&> \p -> do
          -- it (show p) $ testParseHands p >>= \numHands ->
          --   modifyIORef totalHands (+ numHands) `shouldReturn` ()
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


dropBOM bs | BS.take 3 bs == BS.pack [0xEF, 0xBB, 0xBF] = BS.drop 3 bs
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
