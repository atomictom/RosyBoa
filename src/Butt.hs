module Butt where

import Parser
import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import System.IO.Unsafe
import Text.Printf

type Identifier = String
-- type Op = (Math -> Math, String)

data Statement = Print Expression
               | Pass
               | While Expression [Statement]
               | If [(Expression, [Statement])] deriving Show

run :: Statement -> IO ()
run (Pass) = return ()
run (Print e) = case (eval e) of
                  StringLit s -> putStrLn s
                  x -> putStrLn (show x)
run (If []) = return ()
run (If ((cond, stmts):cases)) = case eval cond of
                                   BoolLit True -> mapM_ run stmts
                                   _ -> run (If cases)

statement :: Parser Statement
statement = asum [ Print <$> (literal "print" *> expr)
                 , const Pass <$> literal "pass"
                 , ifStmt ]

ifStmt :: Parser Statement
ifStmt = do
    literal "if"
    cond <- expr
    body <- stmtBody
    return $ If [(cond, body)]

stmtBody :: Parser [Statement]
stmtBody = between (spaced $ literal "{") stmts (spaced $ literal "}") 
  where
    stmts = many (statement <* terminator) 
    terminator =  const () <$> spaced (literal ";")
              <|> const () <$> newline
              <|> const () <$> lookahead (space *> literal "}")

data Expression = FunctionCall Identifier [Expression]
                | Ternary Expression Expression Expression
                | Plus Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Div Expression Expression
                | Power Expression Expression
                | Lt Expression Expression
                | Le Expression Expression
                | Gt Expression Expression
                | Ge Expression Expression
                | Eq Expression Expression
                | Ne Expression Expression
                | And Expression Expression
                | Or Expression Expression
                | Positive Expression
                | Negative Expression
                | Not Expression
                | Lookup Identifier
                | FloatLit Float
                | IntLit Integer
                | StringLit String
                | CharLit Char
                | BoolLit Bool

instance Show Expression where
    show (Ternary e1 e2 e3) = printf "(Ternary %s %s %s)" (show e1) (show e2) (show e3)
    show (Plus e1 e2) = printf "(Plus %s %s)" (show e1) (show e2)
    show (Minus e1 e2) = printf "(Minus %s %s)" (show e1) (show e2)
    show (Mult e1 e2) = printf "(Mult %s %s)" (show e1) (show e2)
    show (Div e1 e2) = printf "(Div %s %s)" (show e1) (show e2)
    show (Power e1 e2) = printf "(Power %s %s)" (show e1) (show e2)
    show (Lt e1 e2) = printf "(Lt %s %s)" (show e1) (show e2)
    show (Le e1 e2) = printf "(Le %s %s)" (show e1) (show e2)
    show (Gt e1 e2) = printf "(Gt %s %s)" (show e1) (show e2)
    show (Ge e1 e2) = printf "(Ge %s %s)" (show e1) (show e2)
    show (Eq e1 e2) = printf "(Eq %s %s)" (show e1) (show e2)
    show (Ne e1 e2) = printf "(Ne %s %s)" (show e1) (show e2)
    show (And e1 e2) = printf "(And %s %s)" (show e1) (show e2)
    show (Or e1 e2) = printf "(Or %s %s)" (show e1) (show e2)
    show (Negative e) = printf "(Negative %s)" (show e)
    show (Positive e) = printf "(Positive %s)" (show e)
    show (Not e) = printf "(Not %s)" (show e)
    show (Lookup id) = printf "(Lookup %s)" id
    show (BoolLit x) = show x
    show (CharLit x) = show x
    show (StringLit x) = show x
    show (FloatLit x) = show x
    show (IntLit x) = show x


eval :: Expression -> Expression
eval (Ternary cond true false) = case eval cond of
                                   BoolLit True -> eval true
                                   BoolLit False -> eval false
eval (Plus e1 e2) = plus (eval e1) (eval e2)
  where
    plus (FloatLit x) (FloatLit y) = FloatLit $ x + y
    plus (FloatLit x) (IntLit y) = FloatLit $ x + (fromIntegral y)
    plus (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) + y
    plus (IntLit x) (IntLit y) = IntLit $ x + y
eval (Minus e1 e2) = minus (eval e1) (eval e2)
  where
    minus (FloatLit x) (FloatLit y) = FloatLit $ x - y
    minus (FloatLit x) (IntLit y) = FloatLit $ x - (fromIntegral y)
    minus (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) - y
    minus (IntLit x) (IntLit y) = IntLit $ x - y
eval (Mult e1 e2) = multiply (eval e1) (eval e2)
  where
    multiply (FloatLit x) (FloatLit y) = FloatLit $ x * y
    multiply (FloatLit x) (IntLit y) = FloatLit $ x * (fromIntegral y)
    multiply (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) * y
    multiply (IntLit x) (IntLit y) = IntLit $ x * y
eval (Div e1 e2) = divide (eval e1) (eval e2)
  where
    divide (FloatLit x) (FloatLit y) = FloatLit $ x / y
    divide (FloatLit x) (IntLit y) = FloatLit $ x / (fromIntegral y)
    divide (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) / y
    divide (IntLit x) (IntLit y) = IntLit $ x `div` y
eval (Power e1 e2) = power (eval e1) (eval e2)
  where
    power (FloatLit x) (FloatLit y) = FloatLit $ x ** y
    power (FloatLit x) (IntLit y) = FloatLit $ x ^ y
    power (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) ** y
    power (IntLit x) (IntLit y) = IntLit $ x ^ y
eval (Lt e1 e2) = lt (eval e1) (eval e2)
  where
    lt (FloatLit x) (FloatLit y) = BoolLit $ x < y
    lt (FloatLit x) (IntLit y) = BoolLit $ x < (fromIntegral y)
    lt (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) < y
    lt (IntLit x) (IntLit y) = BoolLit $ x < y
    lt (StringLit x) (StringLit y) = BoolLit $ x < y
    lt (CharLit x) (CharLit y) = BoolLit $ x < y
    lt (BoolLit x) (BoolLit y) = BoolLit $ x < y
eval (Le e1 e2) = le (eval e1) (eval e2)
  where
    le (FloatLit x) (FloatLit y) = BoolLit $ x <= y
    le (FloatLit x) (IntLit y) = BoolLit $ x <= (fromIntegral y)
    le (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) <= y
    le (IntLit x) (IntLit y) = BoolLit $ x <= y
    le (StringLit x) (StringLit y) = BoolLit $ x <= y
    le (CharLit x) (CharLit y) = BoolLit $ x <= y
    le (BoolLit x) (BoolLit y) = BoolLit $ x <= y
eval (Gt e1 e2) = gt (eval e1) (eval e2)
  where
    gt (FloatLit x) (FloatLit y) = BoolLit $ x > y
    gt (FloatLit x) (IntLit y) = BoolLit $ x > (fromIntegral y)
    gt (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) > y
    gt (IntLit x) (IntLit y) = BoolLit $ x > y
    gt (StringLit x) (StringLit y) = BoolLit $ x > y
    gt (CharLit x) (CharLit y) = BoolLit $ x > y
    gt (BoolLit x) (BoolLit y) = BoolLit $ x > y
eval (Ge e1 e2) = ge (eval e1) (eval e2)
  where
    ge (FloatLit x) (FloatLit y) = BoolLit $ x >= y
    ge (FloatLit x) (IntLit y) = BoolLit $ x >= (fromIntegral y)
    ge (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) >= y
    ge (IntLit x) (IntLit y) = BoolLit $ x >= y
    ge (StringLit x) (StringLit y) = BoolLit $ x >= y
    ge (CharLit x) (CharLit y) = BoolLit $ x >= y
    ge (BoolLit x) (BoolLit y) = BoolLit $ x >= y
eval (Eq e1 e2) = eq (eval e1) (eval e2)
  where
    eq (FloatLit x) (FloatLit y) = BoolLit $ x == y
    eq (FloatLit x) (IntLit y) = BoolLit $ x == (fromIntegral y)
    eq (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) == y
    eq (IntLit x) (IntLit y) = BoolLit $ x == y
    eq (StringLit x) (StringLit y) = BoolLit $ x == y
    eq (CharLit x) (CharLit y) = BoolLit $ x == y
    eq (BoolLit x) (BoolLit y) = BoolLit $ x == y
eval (Ne e1 e2) = ne (eval e1) (eval e2)
  where
    ne (FloatLit x) (FloatLit y) = BoolLit $ x /= y
    ne (FloatLit x) (IntLit y) = BoolLit $ x /= (fromIntegral y)
    ne (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) /= y
    ne (IntLit x) (IntLit y) = BoolLit $ x /= y
    ne (StringLit x) (StringLit y) = BoolLit $ x /= y
    ne (CharLit x) (CharLit y) = BoolLit $ x /= y
    ne (BoolLit x) (BoolLit y) = BoolLit $ x /= y
eval (And e1 e2) = and (eval e1) (eval e2)
  where
    and (BoolLit x) (BoolLit y) = BoolLit $ x && y
eval (Or e1 e2) = or (eval e1) (eval e2)
  where
    or (BoolLit x) (BoolLit y) = BoolLit $ x || y
eval (Positive e1) = positive (eval e1)
  where
    positive (FloatLit x) = FloatLit x
    positive (IntLit x) = IntLit x
eval (Negative e1) = negative (eval e1)
  where
    negative (FloatLit x) = FloatLit (negate x)
    negative (IntLit x) = IntLit (negate x)
eval (Not e1) = negation (eval e1)
  where
    negation (BoolLit x) = BoolLit $ not x
eval (Lookup id) = undefined
eval (FunctionCall id args) = undefined
eval x = x

ternary = do
    literal "if"
    cond <- spaced expr
    literal "then"
    true <- spaced expr
    literal "else"
    false <- spaced expr
    return $ Ternary cond true false

expr :: Parser Expression
expr = ternary <|> condition 

condition :: Parser Expression
condition = orExpr

orExpr :: Parser Expression
orExpr = chainl1 (spaced andExpr) (const Or <$> (literal "or"))

andExpr :: Parser Expression
andExpr = chainl1 (spaced notExpr) (const And <$> (literal "and"))

notExpr :: Parser Expression
notExpr = do
    not <- optional (literal "not")
    expr <- equality
    return $ case not of
               Nothing -> expr
               Just _ -> Not expr

equality :: Parser Expression
equality = chainl1 (spaced comparison) ops 
  where
    ops = asum [ const Eq <$> literal "=="
               , const Ne <$> literal "!=" ]

comparison :: Parser Expression
comparison = chainl1 (spaced mathexpr) ops 
  where
    ops = asum [ const Le <$> literal "<=" 
               , const Ge <$> literal ">="
               , const Gt <$> literal ">"
               , const Lt <$> literal "<" ]

mathexpr :: Parser Expression
mathexpr = chainl1 (spaced term) ops
  where
    ops = asum [ const Plus <$> literal "+"
               , const Minus <$> literal "-" ]

term :: Parser Expression
term = chainl1 (spaced factor) ops
  where
    ops = asum [ const Mult <$> literal "*"
               , const Div <$> literal "/" ]

factor :: Parser Expression
factor = chainr1 (spaced value) (const Power <$> literal "**")

unary :: Parser Expression
unary = unaryOps <*> value
  where
    unaryOps = asum [ const Positive <$> literal "+"
                    , const Negative <$> literal "-" ]

value :: Parser Expression
value = asum [ FloatLit <$> float
             , IntLit <$> integer
             , CharLit <$> character
             , StringLit <$> string
             , boolLit
             , between (char '(') (spaced expr) (char ')') ]

boolLit :: Parser Expression
boolLit = asum [ const (BoolLit True) <$> literal "True"
               , const (BoolLit False) <$> literal "False" ]
