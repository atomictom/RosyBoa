{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative hiding (empty, (<|>), (>>))
import Data.Char
import Parser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.QuickCheck (testProperty, (==>))
import Text.Regex.TDFA ((=~))

main :: IO ()
main = $(defaultMainGenerator)

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft = not . isRight

failRegexpMatch :: String -> Either String b -> Bool
failRegexpMatch r (Left e) = e =~ r
failRegexpMatch _ _ = False

digitInt :: Parser Int
digitInt = fmap digitToInt digit

test_anyChar =
  [ testCase "parseResult success" $
      (parseResult "test" anyChar) @?= (Right 't')

  , testCase "Empty strings give an error (parseResult)" $
      (parseResult "" anyChar) @?= (Left "Expected a character, got nothing.")

  , testCase "parseRemain success" $
      (parseRemain "test" anyChar) @?= Right "est"

  , testProperty "parseResult of anyChar 'x.*' == 'x'" $
      \s -> length s > 0 ==> parseResult s anyChar == Right (head s)

  , testProperty "parseRemain of anyChar 'x.*' == '.*'" $
      \s -> length s > 0 ==> parseRemain s anyChar == Right (tail s)
  ]

test_oneOf =
  [ testCase "parseResult success" $
      parseResult "test" (oneOf "tx") @?= (Right 't')

  , testCase "Empty strings give an error (parseResult)" $
      parseResult "" (oneOf "t") @?=
          Left "Expected one of \"t\", got nothing."

  , testCase "A string without correct leading char gives an error" $
      parseResult "abc" (oneOf "t") @?=
          Left "Expected one of \"t\", got 'a'."

  , testCase "parseRemain success" $
      parseRemain "test" (oneOf "t") @?= Right "est"

  , testProperty "parseResult of oneOf 'x' 'x.*' == 'x'" $
      \s -> parseResult ('x' : s) (oneOf "x") == Right 'x'

  , testProperty "parseRemain of oneOf 'x' 'x.*' == '.*'" $
      \s -> parseRemain ('x' : s) (oneOf "x") == Right s
  ]

test_digit =
    [ testCase "parseResult success" $
        (parseResult "123" digit @?= (Right '1'))

    , testCase "Non-digits fail" $
        parseResult "abc" digit @?=
          Left "Expected one of \"0123456789\", got 'a'."

    , testProperty "parseResult of any (show Int) succeeds" $
        \i -> (i :: Int) >= 0 ==> isRight $ parseResult (show i) digit
    ]

test_upper =
    [ testCase "parseResult success" $
        (parseResult "CAT" upper @?= (Right 'C'))

    , testCase "Non-uppercase leading char fails" $
        isLeft (parseResult "cAT" upper) @?= True
    ]

test_lower =
    [ testCase "parseResult success" $
        (parseResult "cat" lower @?= (Right 'c'))

    , testCase "Non-lowercase leading char fails" $
        assert $ failRegexpMatch ".*abc.*" (parseResult "Cat" lower)
    ]

test_fmap =
    [ testCase "parseResult (fmap isDigit digit) succeeds" $
        parseResult "2" (fmap isDigit digit) @?= Right True

    , testCase "parseResult fmap digit to n+1 succeeds" $
        parseResult "2" (fmap ((+1) . read . return) digit) @?= Right 3

    , testCase "parseResult fmap failure does nothing" $
        assert $ failRegexpMatch ".*123.*" $
          parseResult "cat" (fmap (isDigit) digit)

    , testProperty "fmap id = id" $
        \i -> (i :: Int) >= 0 ==>
          parseResult (show i) (fmap id digit) == parseResult (show i) digit

    , testProperty "fmap (g . f) = fmap g . fmap f" $
        \i -> (i :: Int) >= 0 ==>
          parseResult (show i) (fmap ((+7) . (*3)) digitInt) ==
            parseResult (show i) (fmap (+7) . fmap (*3) $ digitInt)
    ]

test_applicative =
    [ testCase "parseResult pure succeeds" $
        parseResult "2C" (pure 5) @?= Right 5

    , testCase "parseResult ap succeeds" $
        parseResult "24" (pure (+) <*> digitInt <*> digitInt) @?= Right 6

    , testCase "parseResult right ap succeeds" $
        parseResult "2C" (digit *> upper) @?= Right 'C'

    , testCase "parseResult right ap fails first parse" $
        assert $ isLeft (parseResult "2C" (upper *> digit))

    , testCase "parseResult right ap fails first parse" $
        assert $ isLeft (parseResult "2C" (upper *> digit))

    , testCase "parseResult right ap fails second parse" $
        assert $ failRegexpMatch "Expected.*123.*" $
          parseResult "2C" (digit *> digit)
    ]
