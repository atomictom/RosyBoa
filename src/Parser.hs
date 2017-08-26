module Parser where

import Text.Printf
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrorMsg = String

data Parse a = Fail ErrorMsg | Success a String deriving (Show)

data Parser a = Parser
              { parser :: (String -> Parse a) }

parseResult :: String -> Parser a -> Either ErrorMsg a
parseResult s p = case parser p s of
                    Fail e -> Left e
                    Success a s -> Right a

parseRemain :: String -> Parser a -> Either ErrorMsg String
parseRemain s p = case parser p s of
                    Fail e -> Left e
                    Success a s -> Right s

anyChar :: Parser Char
anyChar = Parser parseAnyChar
  where
    parseAnyChar :: String -> Parse Char
    parseAnyChar [] = Fail $ "Expected a character, got nothing."
    parseAnyChar (x:xs) = Success x xs

char :: Char -> Parser Char
char c = Parser parseChar
  where
    parseChar :: String -> Parse Char
    parseChar (x:xs)
      | x == c = Success c xs
      | otherwise = Fail $ printf "Expected '%c', got '%c'." c x
    parseChar _ = Fail $ printf "Expected '%c', got nothing." c

oneOf :: [Char] -> Parser Char
oneOf cs = Parser parseOneOf
  where
    parseOneOf :: String -> Parse Char
    parseOneOf [] = Fail $ printf "Expected one of \"%s\", got nothing." cs
    parseOneOf (x:xs) = if x `elem` cs
                   then Success x xs
                   else Fail $ printf "Expected one of \"%s\", got '%c'." cs x

digit :: Parser Char
digit = oneOf "0123456789"

lower :: Parser Char
lower = oneOf "abcdefghijklmnopqrstuvwxyz"

upper :: Parser Char
upper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

letter :: Parser Char
letter = lower <|> upper <?> "Expected an uppercase or lowercase letter."


identifier :: Parser String
identifier = (:) <$> start <*> body <?> "Expected an identifier."
  where
    start :: Parser Char
    start = letter <|> char '_'
    body :: Parser String
    body = many (start <|> digit)


instance Functor Parse where
    fmap f (Fail x) = Fail x
    fmap f (Success a s) = Success (f a) s

instance Functor Parser where
    fmap f p = Parser (\s -> fmap f ((parser p) s))

instance Applicative Parse where
    pure a = Success a ""
    Success f s <*> Success a s' = Success (f a) s'
    Fail e <*> _ = Fail e
    _ <*> Fail e = Fail e

instance Applicative Parser where
    pure a = Parser (\s -> Success a s)
    f <*> a = Parser $ \s -> case (parser f s) of
                               Fail e -> Fail e
                               Success f s' -> fmap f (parser a s')

instance Alternative Parser where
    empty = Parser $ \s -> Fail ""
    a <|> b = Parser $ \s -> case parser a s of
                               success@(Success _ _) -> success
                               Fail _ -> parser b s

(<?>) :: Parser a -> String -> Parser a
(<?>) p e = Parser $ \s -> case parser p s of
                             Success a s' -> Success a s'
                             Fail _ -> Fail e

-- string :: String -> Parser String
-- string [] = ParseError "Expected a quote, got nothing."
-- string s = case char '"' s of
--              Parser _ r -> parseManyChars r
--              ParseError e -> ParseError e
--   where
--     parseManyChars :: String -> Parser String
--     parseManyChars [] = Parser "" ""
--     parseManyChars (x:xs) = case x of
--                               '"' -> Parser "" xs
--                               _ -> let Parser parsed remaining = parseManyChars xs
--                                    in Parser (x:parsed) remaining
--
-- many :: String -> (String -> Parser String) -> Parser String
-- many s f = case f s of
--              Parser s' r -> many r f
--
-- decimal :: String -> Parser Integer
-- decimal s = oneOf "123456789" >>


-- parseDecimal :: String -> Decimal
-- parseDecimal [] = error "Expected a decimal, got an empty string."
-- parseDecimal "0" = 0
-- parseDecimal (x:xs) = case x of
--                         '0' -> error "Expected [1-9], got 0"
--                         _ -> parseNonZeroDecimal (x:xs)
--   where
--     parseDigit :: Char -> Integer
--     parseDigit x = case x of
--                      '0' -> 0
--                      '1' -> 1
--                      '2' -> 2
--                      '3' -> 3
--                      '4' -> 4
--                      '5' -> 5
--                      '6' -> 6
--                      '7' -> 7
--                      '8' -> 8
--                      '9' -> 9
--                      _ -> error "Expected [0-9], got " ++ [x] ++ "."
--     parseNonZeroDecimal :: String -> Integer
--     parseNonZeroDecimal [] = 0
--     parseNonZeroDecimal (x:[]) = parseDigit x
--     parseNonZeroDecimal (x:xs) = parseDigit x * 10 + parseNonZeroDecimal xs

    -- print $ string "\"99\""
    -- print $ string "\"apple\"pie"
    -- print $ string "\"[test]\""
    -- print $ oneOf "abc" "apple" <|> oneOf "abc" "apple"
    -- print $ oneOf "abc" "apple" <|> oneOf "abc" "xyz"
    -- print $ oneOf "abc" "xyz" <|> oneOf "abc" "apple"
    -- print $ oneOf "abc" "xyz" <|> oneOf "abc" "xyz"
    -- print $ oneOf "abc" "apple" >> char 'p'
    -- print $ oneOf "abc" "apple" >> lower
    -- print $ oneOf "abc" "apple" >> upper
