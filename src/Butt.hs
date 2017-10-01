module Butt where

import Parser
import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Maybe (maybeToList)
import Data.Foldable (asum)
import qualified Data.Map as Map
import System.IO.Unsafe
import Text.Printf

type Identifier = String

type Program = [Statement]
data IfCond = IfCond Expression Program deriving Show
data Statement = Print Expression
               | Pass
               | While Expression Program
               | If [IfCond]
               | Assign Identifier Expression
               deriving Show

data ButtStats = ButtStats Int [String]

type SymbolTable = Map.Map Identifier Expression
type ButtEnv = SymbolTable
type ButtLog = ButtStats
type ButtState = SymbolTable
type ProgramMonad a = RWST ButtEnv ButtLog ButtState IO a

instance Monoid ButtStats where
    mempty = ButtStats 0 []
    mappend (ButtStats i x) (ButtStats j y) = ButtStats (i+j) (x++y)

tick = tell (ButtStats 1 [])
info s = tell (ButtStats 0 [s])

-- Usage: run $ resultOr statement "if True { pass }" Pass

run :: Program -> IO ()
run program = do
    printf "--------------------\n"
    printf "Running program...\n"
    (_, stats) <- execRWST (mapM_ interpret program) Map.empty Map.empty
    let ButtStats ticks logs = stats
    printf "\n"
    printf "--------------------\n"
    printf "Stats:\n"
    printf "Number of ticks: %d\n" ticks
    printf "\n"
    mapM_ (printf "INFO: %s\n") logs
    printf "--------------------\n\n"

interpret :: Statement -> ProgramMonad ()
interpret Pass = do
    tick
    info "pass"
    return ()
interpret (Print e) = do
    tick
    x <- eval e
    liftIO $ putStrLn $ case x of
                          StringLit s -> s
                          x -> (show x)
    return ()
interpret (If []) = do
    tick
    info "if-statement with no matching conditions"
    return ()
interpret (If (c:cs)) = do
    tick
    let IfCond cond stmts = c
    x <- eval cond
    case x of
      BoolLit True -> do
        tick
        info "if-statement condition succeeded"
        mapM_ interpret stmts
      BoolLit False -> do
        tick
        info "if-statement condition failed"
        interpret (If cs)
      _ -> do
        tick
        fail "Condition did not evaluate to a boolean you little dummy"
interpret loop@(While cond stmts) = do
    tick
    info "while-statement"
    x <- eval cond
    case x of
      BoolLit True -> do
        tick
        info "while-statement condition succeeded"
        mapM_ interpret stmts
        interpret loop
      BoolLit False -> do
        tick
        info "while-statement condition failed"
      _ -> do
        tick
        fail "Condition did not evaluate to a boolean dum-dum"
interpret (Assign id expr) = do
    tick
    value <- eval expr
    info $ printf "Inserting %s as %s" id (show value)
    modify (Map.insert id value)

statement :: Parser Statement
statement = asum [ printStmt
                 , passStmt
                 , assignStmt
                 , whileStmt
                 , ifStmt ] <?> "Expected a statement."

printStmt :: Parser Statement
printStmt = Print <$> (literal "print" *> expr)

passStmt :: Parser Statement
passStmt = const Pass <$> literal "pass"

assignStmt :: Parser Statement
assignStmt = Assign <$> identifier <* spaced (literal "=") <*> expr

whileStmt :: Parser Statement
whileStmt = literal "while" *> (While <$> expr <*> stmtBody)

ifStmt :: Parser Statement
ifStmt = do
    x <- ifCond "if"
    xs <- many (ifCond "elif")
    y <- maybeToList <$> optional elseCond
    return $ If (x : (xs ++ y))
  where
    ifCond s = literal s *> (IfCond <$> expr <*> stmtBody)
    elseCond = literal "else" *> (IfCond (BoolLit True) <$> stmtBody)

program :: Parser Program
program = many (statement <* terminator)
  where
    terminator =  const () <$> spaced (literal ";")
              <|> const () <$> newline
              <|> const () <$> lookahead (space *> literal "}")
              <|> lookahead (eof)

stmtBody :: Parser Program
stmtBody = between (spaced $ literal "{") program (spaced $ literal "}")

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

op :: (Expression -> Expression -> Expression)
   -> Expression
   -> Expression
   -> ProgramMonad Expression
op f e1 e2 = do
    x <- eval e1
    y <- eval e2
    return $ f x y

eval :: Expression -> ProgramMonad Expression
eval (Ternary cond true false) = do
    x <- eval cond
    case x of
      BoolLit True -> eval true
      BoolLit False -> eval false
eval (Plus e1 e2) = op plus e1 e2
  where
    plus (FloatLit x) (FloatLit y) = FloatLit $ x + y
    plus (FloatLit x) (IntLit y) = FloatLit $ x + (fromIntegral y)
    plus (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) + y
    plus (IntLit x) (IntLit y) = IntLit $ x + y
eval (Minus e1 e2) = op minus e1 e2
  where
    minus (FloatLit x) (FloatLit y) = FloatLit $ x - y
    minus (FloatLit x) (IntLit y) = FloatLit $ x - (fromIntegral y)
    minus (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) - y
    minus (IntLit x) (IntLit y) = IntLit $ x - y
eval (Mult e1 e2) = op multiply e1 e2
  where
    multiply (FloatLit x) (FloatLit y) = FloatLit $ x * y
    multiply (FloatLit x) (IntLit y) = FloatLit $ x * (fromIntegral y)
    multiply (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) * y
    multiply (IntLit x) (IntLit y) = IntLit $ x * y
eval (Div e1 e2) = op divide e1 e2
  where
    divide (FloatLit x) (FloatLit y) = FloatLit $ x / y
    divide (FloatLit x) (IntLit y) = FloatLit $ x / (fromIntegral y)
    divide (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) / y
    divide (IntLit x) (IntLit y) = IntLit $ x `div` y
eval (Power e1 e2) = op power e1 e2
  where
    power (FloatLit x) (FloatLit y) = FloatLit $ x ** y
    power (FloatLit x) (IntLit y) = FloatLit $ x ^ y
    power (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) ** y
    power (IntLit x) (IntLit y) = IntLit $ x ^ y
eval (Lt e1 e2) = op lt e1 e2
  where
    lt (FloatLit x) (FloatLit y) = BoolLit $ x < y
    lt (FloatLit x) (IntLit y) = BoolLit $ x < (fromIntegral y)
    lt (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) < y
    lt (IntLit x) (IntLit y) = BoolLit $ x < y
    lt (StringLit x) (StringLit y) = BoolLit $ x < y
    lt (CharLit x) (CharLit y) = BoolLit $ x < y
    lt (BoolLit x) (BoolLit y) = BoolLit $ x < y
eval (Le e1 e2) = op le e1 e2
  where
    le (FloatLit x) (FloatLit y) = BoolLit $ x <= y
    le (FloatLit x) (IntLit y) = BoolLit $ x <= (fromIntegral y)
    le (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) <= y
    le (IntLit x) (IntLit y) = BoolLit $ x <= y
    le (StringLit x) (StringLit y) = BoolLit $ x <= y
    le (CharLit x) (CharLit y) = BoolLit $ x <= y
    le (BoolLit x) (BoolLit y) = BoolLit $ x <= y
eval (Gt e1 e2) = op gt e1 e2
  where
    gt (FloatLit x) (FloatLit y) = BoolLit $ x > y
    gt (FloatLit x) (IntLit y) = BoolLit $ x > (fromIntegral y)
    gt (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) > y
    gt (IntLit x) (IntLit y) = BoolLit $ x > y
    gt (StringLit x) (StringLit y) = BoolLit $ x > y
    gt (CharLit x) (CharLit y) = BoolLit $ x > y
    gt (BoolLit x) (BoolLit y) = BoolLit $ x > y
eval (Ge e1 e2) = op ge e1 e2
  where
    ge (FloatLit x) (FloatLit y) = BoolLit $ x >= y
    ge (FloatLit x) (IntLit y) = BoolLit $ x >= (fromIntegral y)
    ge (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) >= y
    ge (IntLit x) (IntLit y) = BoolLit $ x >= y
    ge (StringLit x) (StringLit y) = BoolLit $ x >= y
    ge (CharLit x) (CharLit y) = BoolLit $ x >= y
    ge (BoolLit x) (BoolLit y) = BoolLit $ x >= y
eval (Eq e1 e2) = op eq e1 e2
  where
    eq (FloatLit x) (FloatLit y) = BoolLit $ x == y
    eq (FloatLit x) (IntLit y) = BoolLit $ x == (fromIntegral y)
    eq (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) == y
    eq (IntLit x) (IntLit y) = BoolLit $ x == y
    eq (StringLit x) (StringLit y) = BoolLit $ x == y
    eq (CharLit x) (CharLit y) = BoolLit $ x == y
    eq (BoolLit x) (BoolLit y) = BoolLit $ x == y
eval (Ne e1 e2) = op ne e1 e2
  where
    ne (FloatLit x) (FloatLit y) = BoolLit $ x /= y
    ne (FloatLit x) (IntLit y) = BoolLit $ x /= (fromIntegral y)
    ne (IntLit x) (FloatLit y) = BoolLit $ (fromIntegral x) /= y
    ne (IntLit x) (IntLit y) = BoolLit $ x /= y
    ne (StringLit x) (StringLit y) = BoolLit $ x /= y
    ne (CharLit x) (CharLit y) = BoolLit $ x /= y
    ne (BoolLit x) (BoolLit y) = BoolLit $ x /= y
eval (And e1 e2) = op and e1 e2
  where
    and (BoolLit x) (BoolLit y) = BoolLit $ x && y
eval (Or e1 e2) = op or e1 e2
  where
    or (BoolLit x) (BoolLit y) = BoolLit $ x || y
eval (Positive e1) = do
    x <- eval e1
    return $ positive x
  where
    positive (FloatLit x) = FloatLit x
    positive (IntLit x) = IntLit x
eval (Negative e1) = do
    x <- eval e1
    return $ negative x
  where
    negative (FloatLit x) = FloatLit (negate x)
    negative (IntLit x) = IntLit (negate x)
eval (Not e1) = do
    x <- eval e1
    return $ negation x
  where
    negation (BoolLit x) = BoolLit $ not x
eval (Lookup id) = do
  symbols <- get
  eval (symbols Map.! id)
eval (FunctionCall id args) = return $ undefined
eval x = return x

ternary = do
    literal "if"
    cond <- spaced expr
    literal "then"
    true <- spaced expr
    literal "else"
    false <- spaced expr
    return $ Ternary cond true false

expr :: Parser Expression
expr = ternary <|> condition <?> "Expected an expression."

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
             , Lookup <$> identifier
             , boolLit
             , between (char '(') (spaced expr) (char ')') ]

boolLit :: Parser Expression
boolLit = asum [ const (BoolLit True) <$> literal "True"
               , const (BoolLit False) <$> literal "False" ]
