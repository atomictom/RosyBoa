{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Parser
import Test.QuickCheck.Arbitrary
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

failRegexpMatch :: String -> Parse a -> Bool
failRegexpMatch r (Fail e) = e =~ r
failRegexpMatch _ _ = False

digitInt :: Parser Int
digitInt = fmap digitToInt digit

instance Arbitrary a => Arbitrary (Parse a) where
    arbitrary = do
      x <- arbitrary
      return $ case x of
                 Just a -> Success a ""
                 Nothing -> Fail "You lose."

instance Arbitrary a => Arbitrary (Parser a) where
    arbitrary = arbitrary

test_Functor =
    [ testGroup "Laws"
      [ testProperty "Identity: fmap id = id" $
          \i -> (i :: Int) >= 0 ==>
            parseResult (show i) (fmap id digit) == parseResult (show i) digit

      , testProperty "Composition: fmap (g . f) = fmap g . fmap f" $
          \i -> (i :: Int) >= 0 ==>
            parseResult (show i) (fmap ((+7) . (*3)) digitInt) ==
              parseResult (show i) (fmap (+7) . fmap (*3) $ digitInt)
      ]

    , testCase "parseResult (fmap isDigit digit) succeeds" $
        parseResult "2" (fmap isDigit digit) @?= Right True

    , testCase "parseResult fmap digit to n+1 succeeds" $
        parseResult "2" (fmap ((+1) . read . return) digit) @?= Right 3

    , testCase "parseResult fmap failure does nothing" $
        assert $ failRegexpMatch ".*123.*" $ parser (fmap (isDigit) digit) "cat"
    ]

test_Applicative =
    [ testGroup "Laws"
      [ testProperty "Identity: pure id <*> v = v" $
          \s -> not (null s) ==>
            parser (pure id <*> anyChar) s == parser anyChar s

      , testProperty "Homomorphism: pure f <*> pure x = pure (f x)" $
          \i -> let types = i :: Integer in
            parser (pure (+1) <*> pure i) "" == parser (pure (i + 1)) ""

      , testProperty "Interchange: u <*> pure y = pure ($ y) <*> u" $
          \i j -> let types = (i :: Integer, j :: Integer) in
            parser (pure (*j) <*> pure i) "" ==
              parser (pure ($ i) <*> pure (*j)) ""

      , testProperty
          "Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $
            \i j k -> let u = pure (* (i :: Integer))
                          v = pure (+ (j :: Integer))
                          w = pure (k :: Integer) in
              parser (pure (.) <*> u <*> v <*> w) "" ==
              parser (u <*> (v <*> w)) ""
      ]

    , testCase "parseResult pure succeeds" $
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
          parser (digit *> digit) "2C"
    ]

test_Alternative =
    [ testGroup "Laws"
      [ testProperty "Left Identity: empty <|> x = x" $
          \i -> (i :: Integer) >= 0 ==>
            parser (empty <|> digit) (show i) == parser digit (show i)

      , testProperty "Right Identity: x <|> empty = x" $
          \i -> (i :: Integer) >= 0 ==>
            parser (digit <|> empty) (show i) == parser digit (show i)

      , testProperty "Associativity: x <|> (y <|> z) = (x <|> y) <|> z" $
          \s -> parser (digit <|> (upper <|> lower)) s ==
                parser ((digit <|> upper) <|> lower) s
      ]

    , testCase "First parser succeeds" $
        parser (digit <|> upper) "2" @?= Success '2' ""

    , testCase "Second parser succeeds" $
        parser (upper <|> digit) "2" @?= Success '2' ""
    ]

test_Monad =
    [ testGroup "Laws"
      [ testProperty "Left Identity: return a >>= f = f a" $
          \i -> let types = i :: Integer
                    f = \x -> if x >= 0 then return x else empty in
            parser (return i >>= f) "" == parser (f i) ""

      , testProperty "Right Identity: m >>= return = m" $
          \p -> let m = Parser $ \s -> (p :: Parse Integer) in
            parser (m >>= return) "" == parser m ""

      , testProperty
          "Associativity: ((m >>= f) >>= g) = m >>= (\\x -> f x >>= g)" $
            \p -> let m = Parser $ \s -> (p :: Parse Integer)
                      f = \x -> if x >= 0 then return x else empty
                      g = \x -> if even x then return x else empty in
              parser ((m >>= f) >>= g) "" == parser (m >>= (\x -> f x >>= g)) ""
      ]

    , testCase "Parse Int Success" $
        let int_add = do x <- integer
                         space
                         literal "+"
                         space
                         y <- integer
                         return (x + y) in
       parser int_add "222 + 333" @?= Success 555 ""


    , testCase "Parse Int Fails" $
        let int_add = do x <- integer
                         space
                         literal "+"
                         space
                         y <- integer
                         return (x + y) in
       assert $ failRegexpMatch "Expected.*literal.*\\+.*" $
         parser int_add "222 - 333"
    ]

test_MonadPlus =
    [ testGroup "Laws"
      [ testProperty "Left Zero: mzero >>= f = mzero" $
          \i -> let f = const $ if (i :: Int) >= 0 then return i else mzero in
            parser (mzero >>= f) "" == parser mzero ""

      , testProperty "Right Zero: m >> mzero = mzero" $
          \p -> let m = Parser $ \_ -> (p :: Parse Integer) in
            parser (m >> mzero <?> "") "" == (parser mzero "" :: Parse Int)
      ]
    ]

test_float = map floatTest floatCases
  where
    floatTest (spec, Right result) = testCase ("Success Case: " ++ spec) $
      parser float spec @?= Success result ""
    floatTest (spec, Left regexp) = testCase ("Failure Case: " ++ spec) $
      assert (failRegexpMatch regexp (parser float spec))
    floatCases :: [(String, Either String Float)]
    floatCases =
      [ ("0.0", Right 0.0)
      , ("-0", Right 0.0)
      , ("-.0", Right 0.0)
      , ("-.1", Right (-0.1))
      , ("1.0", Right 1.0)
      , ("0f", Right 0.0)
      , ("1f", Right 1.0)
      , ("-1f", Right (-1.0))
      , ("1.0f", Right 1.0)
      , (".0", Right 0.0)
      , (".1", Right 0.1)
      , (".1f", Right 0.1)
      , ("1e6", Right 1000000.0)
      , (".1e6", Right 100000.0)
      , ("1.15e6", Right 1150000.0)
      , ("1e-6", Right 0.0000001)
      , ("1.1e-6",Right  0.00000011)
      , (".1e-6", Right 0.0000001)
      , ("-1.1e-6f", Right (-0.00000011))
      , ("Infinity", Right (read "Infinity"))
      , ("-Infinity", Right (read "-Infinity"))
      , ("-Infinity", Right (read "-Infinity"))
      , ("NaN", Right (read "NaN"))
      , ("-NaN", Right (read "NaN"))
      , ("nan", Left ".*")
      , ("infinity", Left ".*")
      , ("-infinity", Left ".*")
      , ("- Infinity", Left ".*")
      , ("f", Left ".*")
      , (".", Left ".*")
      , ("-.", Left ".*")
      ]


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
        assert $ failRegexpMatch ".*abc.*" (parser lower "Cat")
    ]
