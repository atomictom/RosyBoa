{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty.QuickCheck (testProperty, (==>))
import Parser

main :: IO ()
main = $(defaultMainGenerator)

test_anyChar =
  [ testCase "result success" $
      (parseResult "test" anyChar) @?= (Right 't')
  , testCase "result failure" $
      (parseResult "" anyChar) @?= (Left "Expected a character, got nothing.")
  , testCase "remain success" $
      (parseRemain "test" anyChar) @?= Right "est"
  , testCase "remain failure" $
      (parseRemain "" anyChar) @?= Left  "Expected a character, got nothing."
  , testProperty "result of anyChar 'x.*' == 'x'" $
      \s -> length s > 0 ==> parseResult s anyChar == Right (head s)
  , testProperty "remain of anyChar 'x.*' == '.*'" $
      \s -> length s > 0 ==> parseRemain s anyChar == Right (tail s)
  ]

test_oneOf =
  [ testCase "result success" $
      parseResult "test" (oneOf "t") @?= (Right 't')
  , testCase "result failure empty" $
      parseResult "" (oneOf "t") @?=
          Left "Expected one of \"t\", got nothing."
  , testCase "result failure non-empty" $
      parseResult "abc" (oneOf "t") @?=
          Left "Expected one of \"t\", got 'a'."
  , testCase "remain success" $
      parseRemain "test" (oneOf "t") @?= Right "est"
  , testCase "remain failure empty" $
      parseRemain "" (oneOf "t") @?=
          Left  "Expected one of \"t\", got nothing."
  , testCase "remain failure non-empty" $
      parseRemain "abc" (oneOf "t") @?=
          Left  "Expected one of \"t\", got 'a'."
  , testProperty "result of oneOf 'x' 'x.*' == 'x'" $
      \s -> parseResult ('x' : s) (oneOf "x") == Right 'x'
  , testProperty "remain of oneOf 'x' 'x.*' == '.*'" $
      \s -> parseRemain ('x' : s) (oneOf "x") == Right s
  ]

-- test_digit =
--     [ testCase "result success" $
--
--     ,
--     ]
