module Parser where

import Numbers
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe
import System.IO.Unsafe
import Text.Printf
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

type ErrorMsg = String

data Parse a = Fail ErrorMsg | Success a String deriving (Eq, Show)

data Parser a = Parser
              { parser :: (String -> Parse a) }

resultOr :: Parser a -> String -> a -> a
resultOr p s d = case parser p s of
                   Success a s -> a
                   Fail e -> d

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

failure :: String -> Parser ()
failure s = empty <?> s

anyChar :: Parser Char
anyChar = Parser parseAnyChar
  where
    parseAnyChar :: String -> Parse Char
    parseAnyChar [] = Fail $ "Expected a character, got nothing."
    parseAnyChar (x:xs) = Success x xs

anyBut :: [Char] -> Parser Char
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

optionalNegation :: Num a => Parser (a -> a)
optionalNegation = maybeNegate <$> optional (literal "-")
  where
    maybeNegate (Just _) = negate
    maybeNegate _ = id

float :: Parser Float
float = optionalNegation <*> (infinity <|> nan <|> num)
  where
    infinity = const (read "Infinity") <$> literal "Infinity"
    nan = const (read "NaN") <$> literal "NaN"
    num = do
      first <- many digit
      point <- maybe "" pure <$> optional (char '.')
      second <- many digit
      exp <- optional $ do
        char 'e'
        n <- maybe "" pure <$> optional (char '-')
        e <- some digit
        return (n ++ e)
      f <- maybe "" pure <$> optional (char 'f')
      let hasPointOrExponentOrF = not (null point && isNothing exp && null f)
          hasNumber = not (null first && null second)
      guard $ hasNumber && hasPointOrExponentOrF
      return $ read ("0" ++ first ++ point ++ second ++ "e" ++ maybe "0" id exp)

integer :: Parser Integer
integer = optionalNegation <*> (binary <|> octal <|> hexadecimal <|> decimal)

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

whitespaceChars :: [Char]
whitespaceChars = " \t\n\r"

string :: Parser String
string = between (char '"') (many (escaped <|> anyBut "\"")) (char '"')
  where
    escaped = escapedChar <$> (literal "\\" *> anyChar)

word :: Parser String
word = some (anyBut whitespaceChars)

space :: Parser ()
space = const () <$> many (oneOf " \t")

anySpace :: Parser ()
anySpace = const () <$> many (oneOf " \t\n\r")

spaced :: Parser a -> Parser a
spaced a = space *> a <* space

ignored :: Parser ()
ignored = (const () <$> many comment) <|> space

newline :: Parser ()
newline = const () <$> oneOf "\n\r"

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

try :: Parser a -> Parser (Maybe a)
try a = (Just <$> a) <|> (return Nothing)

eol :: Parser ()
eol = const () <$> (many (oneOf " \t") *> oneOf "\n\r")

anyUntil :: String -> Parser String
anyUntil s = end <|> more <?> failure
  where
    end = fmap (const "") (literal s)
    more = ((:) <$> anyChar <*> anyUntil s)
    failure = printf "Expected to find literal \"%s\", but did not." s

comment :: Parser String
comment = space *> (singleLineComment <|> multiLineComment)
  where
    singleLineComment = literal "--" *> many (anyBut "\n\r") <* lookahead newline
    multiLineComment = literal "{-" *> anyUntil "-}"

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep p = do
    m <- optional p
    case m of
      Nothing -> return []
      Just a -> (a:) <$> ((sep *> sepBy sep p) <|> return [])

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
    x <- p
    r <- optional ((,) <$> op <*> chainr1 p op)
    return $ case r of
               Nothing -> x
               Just (g, y) -> g x y

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
    x <- p
    chainl1' x
  where
    chainl1' x = do
      r <- optional ((,) <$> op <*> p)
      case r of
        Nothing -> return x
        Just (g, y) -> chainl1' (g x y)
