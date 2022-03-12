module RosyBoa where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.Trans.Maybe
import           Data.Foldable             (asum)
import           Data.List                 (intercalate, nub)
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Maybe                (maybeToList)
import           Parser
import           System.IO.Unsafe
import           Text.Printf

-- run $ resultOr (program <* eof) "def test (a, b) { print \"Hello Map Reduce. \" + a + \" \" + b + \"!\" } ; test(\"Baby\", \"RosyBoa\")" [Pass]

runTest :: String -> IO ()
runTest s = run $ resultOr (program <* eof) s [Print (StringLit "Fail")]

showSymbols :: SymbolTable -> String
showSymbols sym = Map.foldlWithKey (\s k x -> s ++ printf "  %s: %s\n" k (show x)) "\n" sym

type Identifier = String


type Program = [Statement]
data IfCond = IfCond Expression Program deriving Show
data Statement = Print Expression
               | Pass
               | While Expression Program
               | If [IfCond]
               | Assign Identifier Expression
               | Return Expression
               | FunctionDef Identifier [Identifier] Program
               | CallStmt Identifier [Expression]
               deriving Show

data RosyBoaStats = RosyBoaStats Int [String]

type SymbolTable = Map.Map Identifier Expression
type RosyBoaEnv = SymbolTable
type RosyBoaLog = RosyBoaStats
type RosyBoaState = SymbolTable
type ProgramMonad a = RWST RosyBoaEnv RosyBoaLog RosyBoaState IO a

instance Monoid RosyBoaStats where
    mempty = RosyBoaStats 0 []
    mappend (RosyBoaStats i x) (RosyBoaStats j y) = RosyBoaStats (i+j) (x++y)

tick :: ProgramMonad ()
tick = tell (RosyBoaStats 1 [])

info :: String -> ProgramMonad ()
-- info s = tell (RosyBoaStats 0 [s]) >> liftIO (putStrLn s)

info s = return ()

getSymbol :: SymbolTable -> Identifier -> ProgramMonad Expression
getSymbol t s = do
    info $ printf "Looking up %s in symbol table: %s" s (showSymbols t)
    case Map.lookup s t of
      Nothing -> fail $ printf "Did not find %s in symbol table: %s" s (showSymbols t)
      Just e -> info (printf "Returning %s for symbol %s" (show e) s) >> return e

-- Usage: run $ resultOr statement "if True { pass }" Pass

run :: Program -> IO ()
run program = do
    printf "--------------------\n"
    printf "Running program...\n"
    (_, stats) <- execRWST (runMaybeT $ asum $ map (MaybeT . interpret) program) Map.empty Map.empty
    let RosyBoaStats ticks logs = stats
    printf "\n"
    printf "--------------------\n"
    printf "Stats:\n"
    printf "Number of ticks: %d\n" ticks
    printf "\n"
    mapM_ (printf "INFO: %s\n") logs
    printf "--------------------\n\n"

interpret :: Statement -> ProgramMonad (Maybe Expression)
interpret Pass = do
    tick
    info "pass"
    return Nothing
interpret (Print e) = do
    tick
    x <- eval e
    liftIO $ putStr $ case x of
                        StringLit s -> s
                        CharLit c   -> [c]
                        x           -> (show x)
    return Nothing
interpret (If []) = do
    tick
    info "if-statement with no matching conditions"
    return Nothing
interpret (If (c:cs)) = do
    tick
    let IfCond cond stmts = c
    x <- eval cond
    case x of
      BoolLit True -> do
        tick
        info "if-statement condition succeeded"
        mapM_ interpret stmts
        return Nothing
      BoolLit False -> do
        tick
        info "if-statement condition failed"
        interpret (If cs)
        return Nothing
      _ -> do
        tick
        fail "Condition did not evaluate to a boolean you little dummy"
        return Nothing
interpret loop@(While cond stmts) = do
    tick
    info "while-statement"
    x <- eval cond
    case x of
      BoolLit True -> do
        tick
        info "while-statement condition succeeded"
        runMaybeT $ asum $ map (MaybeT . interpret) stmts
        interpret loop
        return Nothing
      BoolLit False -> do
        tick
        info "while-statement condition failed"
        return Nothing
      _ -> do
        tick
        fail "Condition did not evaluate to a boolean dum-dum"
        return Nothing
interpret (Assign id expr) = do
    tick
    value <- eval expr
    info $ printf "Inserting variable %s as %s" id (show value)
    modify (Map.insert id value)
    args <- ask
    env <- get
    let symbols = Map.union args env
    info $ printf "Symbol Table is now: %s" (showSymbols symbols)
    return Nothing
interpret (FunctionDef id args stmts) = do
    tick
    if nub args /= args
      then fail "There are repeated names in the arg list"
      else return Nothing
    let value = FunctionLit id args stmts
    info $ printf "Inserting function %s as %s" id (show value)
    modify (Map.insert id value)
    return Nothing
interpret (CallStmt id arg_exprs) = do
    tick
    args <- ask
    env <- get
    let symbols = Map.union args env
    FunctionLit _ arg_names stmts <- getSymbol symbols id
    if length arg_exprs /= length arg_names
      then fail "Not all required args were provided" -- TODO add more detail
      else return Nothing
    arg_values <- mapM eval arg_exprs
    info $ printf "Calling function %s with values [%s]" id (intercalate "," (map show arg_values))
    let symbols' = Map.fromList (zip arg_names arg_values)
    local (const symbols') (runMaybeT $ asum $ map (MaybeT . interpret) stmts)
    return Nothing
interpret (Return expr) = do
    tick
    value <- eval expr
    info $ printf "Returning %s" (show value)
    return (Just value)

statement :: Parser Statement
statement = asum  [ printStmt
                  , passStmt
                  , assignStmt
                  , whileStmt
                  , retStmt
                  , functionDef
                  , callStmt
                  , ifStmt
                  , const Pass <$> comment]
                  <?> "Expected a statement."

printStmt :: Parser Statement
printStmt = Print <$> (literal "print" *> expr)

passStmt :: Parser Statement
passStmt = const Pass <$> literal "pass"

assignStmt :: Parser Statement
assignStmt = Assign <$> identifier <* spaced (literal "=") <*> expr

whileStmt :: Parser Statement
whileStmt = literal "while" *> (While <$> expr <*> stmtBody)

retStmt :: Parser Statement
retStmt = literal "return" *> (Return <$> expr)

ifStmt :: Parser Statement
ifStmt = do
    x <- ifCond "if"
    xs <- many (anySpace *> ifCond "elif")
    y <- maybeToList <$> optional elseCond
    return $ If (x : (xs ++ y))
  where
    ifCond s = literal s *> (IfCond <$> expr <*> stmtBody)
    elseCond = anySpace *> literal "else" *> (IfCond (BoolLit True) <$> stmtBody)

functionDef :: Parser Statement
functionDef = do
    literal "def"
    space
    id <- identifier
    space
    args <- between (space *> literal "(") argList (space *> literal ")")
    body <- stmtBody
    return $ FunctionDef id args body
  where
    argList = sepBy (literal ",") (space *> identifier)

callStmt :: Parser Statement
callStmt = do
    id <- identifier
    space
    args <- between (space *> literal "(") argList (space *> literal ")")
    return $ CallStmt id args
  where
    argList = sepBy (literal ",") (space *> expr)

program :: Parser Program
program = many (anySpace *> statement <* (const () <$> optional comment) <* terminator)
  where
    terminator =  const () <$> (space *> literal ";")
              <|> const () <$> eol
              <|> const () <$> (space *> lookahead (literal "}"))
              <|> eof

stmtBody :: Parser Program
stmtBody = between (anySpace *> literal "{") program (anySpace *> literal "}")

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
                | FunctionLit Identifier [Identifier] Program
                | FloatLit Float
                | IntLit Integer
                | StringLit String
                | CharLit Char
                | BoolLit Bool

instance Show Expression where
    show (FunctionCall id args) = printf "(FunctionCall %s [%s])" id (intercalate "," (map show args))
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
    show (FunctionLit id args stmts) =
        let show_args = intercalate "," (map show args)
            show_program = intercalate "," (map show stmts)
            in printf "(FunctionCall %s [%s] {%s})" id show_args show_program
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
      BoolLit True  -> eval true
      BoolLit False -> eval false
eval (Plus e1 e2) = op plus e1 e2
  where
    plus (FloatLit x) (FloatLit y)   = FloatLit $ x + y
    plus (FloatLit x) (IntLit y)     = FloatLit $ x + (fromIntegral y)
    plus (IntLit x) (FloatLit y)     = FloatLit $ (fromIntegral x) + y
    plus (IntLit x) (IntLit y)       = IntLit $ x + y
    plus (StringLit x) (StringLit y) = StringLit $ x ++ y
    plus (StringLit x) y             = StringLit $ x ++ (show y)
    plus x (StringLit y)             = StringLit $ (show x) ++ y
eval (Minus e1 e2) = op minus e1 e2
  where
    minus (FloatLit x) (FloatLit y) = FloatLit $ x - y
    minus (FloatLit x) (IntLit y)   = FloatLit $ x - (fromIntegral y)
    minus (IntLit x) (FloatLit y)   = FloatLit $ (fromIntegral x) - y
    minus (IntLit x) (IntLit y)     = IntLit $ x - y
eval (Mult e1 e2) = op multiply e1 e2
  where
    multiply (FloatLit x) (FloatLit y) = FloatLit $ x * y
    multiply (FloatLit x) (IntLit y)   = FloatLit $ x * (fromIntegral y)
    multiply (IntLit x) (FloatLit y)   = FloatLit $ (fromIntegral x) * y
    multiply (IntLit x) (IntLit y)     = IntLit $ x * y
eval (Div e1 e2) = op divide e1 e2
  where
    divide (FloatLit x) (FloatLit y) = FloatLit $ x / y
    divide (FloatLit x) (IntLit y)   = FloatLit $ x / (fromIntegral y)
    divide (IntLit x) (FloatLit y)   = FloatLit $ (fromIntegral x) / y
    divide (IntLit x) (IntLit y)     = IntLit $ x `div` y
eval (Power e1 e2) = op power e1 e2
  where
    power (FloatLit x) (FloatLit y) = FloatLit $ x ** y
    power (FloatLit x) (IntLit y)   = FloatLit $ x ^ y
    power (IntLit x) (FloatLit y)   = FloatLit $ (fromIntegral x) ** y
    power (IntLit x) (IntLit y)     = IntLit $ x ^ y
eval (Lt e1 e2) = op lt e1 e2
  where
    lt (FloatLit x) (FloatLit y)   = BoolLit $ x < y
    lt (FloatLit x) (IntLit y)     = BoolLit $ x < (fromIntegral y)
    lt (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) < y
    lt (IntLit x) (IntLit y)       = BoolLit $ x < y
    lt (StringLit x) (StringLit y) = BoolLit $ x < y
    lt (CharLit x) (CharLit y)     = BoolLit $ x < y
    lt (BoolLit x) (BoolLit y)     = BoolLit $ x < y
eval (Le e1 e2) = op le e1 e2
  where
    le (FloatLit x) (FloatLit y)   = BoolLit $ x <= y
    le (FloatLit x) (IntLit y)     = BoolLit $ x <= (fromIntegral y)
    le (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) <= y
    le (IntLit x) (IntLit y)       = BoolLit $ x <= y
    le (StringLit x) (StringLit y) = BoolLit $ x <= y
    le (CharLit x) (CharLit y)     = BoolLit $ x <= y
    le (BoolLit x) (BoolLit y)     = BoolLit $ x <= y
eval (Gt e1 e2) = op gt e1 e2
  where
    gt (FloatLit x) (FloatLit y)   = BoolLit $ x > y
    gt (FloatLit x) (IntLit y)     = BoolLit $ x > (fromIntegral y)
    gt (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) > y
    gt (IntLit x) (IntLit y)       = BoolLit $ x > y
    gt (StringLit x) (StringLit y) = BoolLit $ x > y
    gt (CharLit x) (CharLit y)     = BoolLit $ x > y
    gt (BoolLit x) (BoolLit y)     = BoolLit $ x > y
eval (Ge e1 e2) = op ge e1 e2
  where
    ge (FloatLit x) (FloatLit y)   = BoolLit $ x >= y
    ge (FloatLit x) (IntLit y)     = BoolLit $ x >= (fromIntegral y)
    ge (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) >= y
    ge (IntLit x) (IntLit y)       = BoolLit $ x >= y
    ge (StringLit x) (StringLit y) = BoolLit $ x >= y
    ge (CharLit x) (CharLit y)     = BoolLit $ x >= y
    ge (BoolLit x) (BoolLit y)     = BoolLit $ x >= y
eval (Eq e1 e2) = op eq e1 e2
  where
    eq (FloatLit x) (FloatLit y)   = BoolLit $ x == y
    eq (FloatLit x) (IntLit y)     = BoolLit $ x == (fromIntegral y)
    eq (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) == y
    eq (IntLit x) (IntLit y)       = BoolLit $ x == y
    eq (StringLit x) (StringLit y) = BoolLit $ x == y
    eq (CharLit x) (CharLit y)     = BoolLit $ x == y
    eq (BoolLit x) (BoolLit y)     = BoolLit $ x == y
eval (Ne e1 e2) = op ne e1 e2
  where
    ne (FloatLit x) (FloatLit y)   = BoolLit $ x /= y
    ne (FloatLit x) (IntLit y)     = BoolLit $ x /= (fromIntegral y)
    ne (IntLit x) (FloatLit y)     = BoolLit $ (fromIntegral x) /= y
    ne (IntLit x) (IntLit y)       = BoolLit $ x /= y
    ne (StringLit x) (StringLit y) = BoolLit $ x /= y
    ne (CharLit x) (CharLit y)     = BoolLit $ x /= y
    ne (BoolLit x) (BoolLit y)     = BoolLit $ x /= y
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
    positive (IntLit x)   = IntLit x
eval (Negative e1) = do
    x <- eval e1
    return $ negative x
  where
    negative (FloatLit x) = FloatLit (negate x)
    negative (IntLit x)   = IntLit (negate x)
eval (Not e1) = do
    x <- eval e1
    return $ negation x
  where
    negation (BoolLit x) = BoolLit $ not x
eval (Lookup id) = do
  args <- ask
  env <- get
  let symbols = Map.union env args
  getSymbol symbols id >>= eval
eval (FunctionCall id arg_exprs) = do
    args <- ask
    env <- get
    let symbols = Map.union env args
    FunctionLit _ arg_names stmts <- getSymbol symbols id
    if length arg_exprs /= length arg_names
      then fail "Not all required args were provided" -- TODO add more detail
      else return ()
    arg_values <- mapM eval arg_exprs
    let args' = Map.fromList (zip arg_names arg_values) -- Fill in function args
    put Map.empty -- Clear the environment
    info $ printf "Using new symbol table: %s" (showSymbols args')
    result <- local (const args') (runMaybeT $ asum $ map (MaybeT . interpret) stmts)
    put env -- Restore the environment
    case result of
      Just x  -> return x
      Nothing -> fail "Function did not return a value"
eval x = return x

functionCall :: Parser Expression
functionCall = do
    id <- identifier
    space
    args <- between (space *> literal "(") argList (space *> literal ")")
    return $ FunctionCall id args
  where
    argList = sepBy (literal ",") (space *> expr)

ternary :: Parser Expression
ternary = do
    space
    literal "if"
    space
    cond <- expr
    space
    literal "then"
    space
    true <- expr
    space
    literal "else"
    space
    false <- expr
    return $ Ternary cond true false

expr :: Parser Expression
expr = spaced (ternary <|> condition <?> "Expected an expression.")

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
               Just _  -> Not expr

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
             , functionCall
             , Lookup <$> identifier
             , between (char '(') (spaced expr) (char ')') ]

boolLit :: Parser Expression
boolLit = asum [ const (BoolLit True) <$> literal "True"
               , const (BoolLit False) <$> literal "False" ]
