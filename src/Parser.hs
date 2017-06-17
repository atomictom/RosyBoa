module Parser where

import Control.Applicative hiding (empty, (<|>), (>>))
import Control.Exception
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrorMsg = String

data Parse a = Fail ErrorMsg | Success a String deriving (Show)
data Parser a = Parser
              { parse :: (String -> Parse a) }

parseResult :: String -> Parser a -> Either ErrorMsg a
parseResult s p = case parse p s of
                    Fail e -> Left e
                    Success a s -> Right a

parseRemain :: String -> Parser a -> Either ErrorMsg String
parseRemain s p = case parse p s of
                    Fail e -> Left e
                    Success a s -> Right s

anyChar :: Parser Char
anyChar = Parser parseChar
  where
    parseChar :: String -> Parse Char
    parseChar [] = Fail $ "Expected a character, got nothing."
    parseChar (x:xs) = Success x xs

oneOf :: [Char] -> Parser Char
oneOf cs = Parser parseOneOf
  where
    parseOneOf :: String -> Parse Char
    parseOneOf [] = Fail $ "Expected one of \"" ++ cs ++ "\", got nothing."
    parseOneOf (x:xs) = if Set.member x (Set.fromList cs)
                   then Success x xs
                   else Fail $ "Expected one of \"" ++ cs ++ "\", got '" ++ [x] ++ "'."
                   --
digit :: Parser Char
digit = oneOf "0123456789"

lower :: Parser Char
lower = oneOf "abcdefghijklmnopqrstuvwxyz"

upper :: Parser Char
upper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- (<|>) :: Parser a -> Parser a -> Parser a
-- (<|>) (ParseError _) b@(Parser _ _ _) = b
-- (<|>) a _ = a
-- infixr 1 <|>
--
-- (>>) :: Parser a -> (String -> Parser b) -> Parser b
-- (>>) (ParseError e) _ = ParseError e
-- (>>) (Parser _ r) f = f r
-- infixr 5 >>
--
--
-- -- identifier = oneOf ""
--
-- char :: Char -> String -> Parser Char
-- char c [] = ParseError $ "Expected '" ++ [c] ++ "', got nothing."
-- char c (x:xs) = case x of
--                   c -> Parser c xs
--                   _ -> ParseError $ "Expected, '" ++ [c] ++ "', got '" ++ [x] ++ "'."
--
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
