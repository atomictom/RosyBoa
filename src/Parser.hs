module Parser where

import Numbers
import Text.Printf
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrorMsg = String

data Parse a = Fail ErrorMsg | Success a String deriving (Eq, Show)

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
                               Success f s' -> fmap f (parser a s')
                               Fail e -> Fail e

instance Alternative Parser where
    empty = Parser $ \s -> Fail ""
    a <|> b = Parser $ \s -> case parser a s of
                               success@(Success _ _) -> success
                               Fail _ -> parser b s

instance Monad Parse where
    return = pure
    Success a s >>= f = case f a of
                          Success b _ -> Success b s
                          Fail e -> Fail e
    Fail e >>= f = Fail e

instance Monad Parser where
    return = pure
    m >>= f = Parser $ \s -> case parser m s of
                               Success a s' -> parser (f a) s'
                               Fail e -> Fail e

instance MonadPlus Parser where
    mzero = empty
    mplus = (<|>)

(<?>) :: Parser a -> String -> Parser a
(<?>) p e = Parser $ \s -> case parser p s of
                             Success a s' -> Success a s'
                             Fail _ -> Fail e

infix 0 <?>

fail :: String -> Parser ()
fail s = empty <?> s

anyChar :: Parser Char
anyChar = Parser parseAnyChar
  where
    parseAnyChar :: String -> Parse Char
    parseAnyChar [] = Fail $ "Expected a character, got nothing."
    parseAnyChar (x:xs) = Success x xs

anyBut :: String -> Parser Char
anyBut but = Parser parseAnyBut
  where
    parseAnyBut :: String -> Parse Char
    parseAnyBut [] = Fail $ "Expected a character, got nothing."
    parseAnyBut (x:xs)
      | x `elem` but = Fail $
          printf "Expected any char but one of %s, got %c" but x
      | otherwise = Success x xs

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

character :: Parser Char
character = between (literal "'") (escaped <|> anyChar) (literal "'")
  where
    escaped = escapedChar <$> (literal "\\" *> anyChar)

eof :: Parser ()
eof = Parser $ \s -> if null s then Success () "" else Fail "Expected EOF."

integer :: Parser Integer
integer = binary <|> octal <|> hexadecimal <|> decimal

binary :: Parser Integer
binary = literal "0b" *> (readIntegerBase 2 <$> some (oneOf "01"))

octal :: Parser Integer
octal = literal "0" *> oneOf "oO" *> (readIntegerBase 8 <$> some octalDigits)
  where
    octalDigits = oneOf "01234567"

decimal :: Parser Integer
decimal = readIntegerBase 10 <$> some digit

hexadecimal :: Parser Integer
hexadecimal = literal "0x" *> (readIntegerBase 16 <$> some hexDigits)
  where
    hexDigits = oneOf "0123456789aAbBcCdDeEfF"

between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = a *> b <* c

escapedChar :: Char -> Char
escapedChar 't' = '\t'
escapedChar 'n' = '\n'
escapedChar 'r' = '\r'
escapedChar '0' = '\0'
escapedChar c = c

string :: Parser String
string = between (char '"') (many (escaped <|> anyBut "\"")) (char '"')
  where
    escaped = escapedChar <$> (literal "\\" *> anyChar)

space :: Parser ()
space = const () <$> many (oneOf " \t\n\r")

literal :: String -> Parser String
literal l = Parser parseLiteral
  where
    parseLiteral :: String -> Parse String
    parseLiteral s
      | l `List.isPrefixOf` s = Success l (drop (length l) s)
      | otherwise = Fail $ printf "Expected to find literal \"%s\"." l

lookahead :: Parser a -> Parser ()
lookahead p = Parser $ \s -> case parser p s of
                               Success _ _ -> Success () s
                               Fail e -> Fail e

anyUntil :: String -> Parser String
anyUntil s = end <|> more <?> failure
  where
    end = fmap (const "") (literal s)
    more = ((:) <$> anyChar <*> anyUntil s)
    failure = printf "Expected to find literal \"%s\", but did not." s

comment :: Parser String
comment = space *> (singleLineComment <|> multiLineComment)
  where
    singleLineComment = literal "--" *> many (anyBut "\n\r")
    multiLineComment = literal "{-" *> anyUntil "-}"
