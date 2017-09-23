module Expr where

import Parser
import Control.Applicative
import Control.Monad
import Data.Foldable
import System.IO.Unsafe
import Text.Printf

type BinOp = (MathExpr -> MathExpr -> MathExpr, String)
type Op = (MathExpr -> MathExpr, String)

data MathExpr = Binary BinOp MathExpr MathExpr
          | Unary Op MathExpr
          | FloatLit Float
          | IntLit Integer
          | Print MathExpr MathExpr

instance Show MathExpr where
    show (Binary (_, opName) x y) =
      printf "Binary (%s `%s` %s)" (show x) opName (show y)
    show (Unary (_, opName) x) = printf "Unary (`%s` %s)" opName (show x)
    show (FloatLit f) = show f
    show (IntLit i) = show i
    show (Print e1 e2) = printf "Print %s -> %s" (show e1) (show e2)

mkOp :: Char -> Op
mkOp '+' = (id, "+")
mkOp '-' = (negater, "-")
  where
    negater (FloatLit f) = FloatLit $ negate f
    negater (IntLit i) = IntLit $ negate i

mkBinOp :: Char -> BinOp
mkBinOp '+' = (plus, "+")
  where
    plus (FloatLit x) (FloatLit y) = FloatLit $ x + y
    plus (FloatLit x) (IntLit y) = FloatLit $ x + (fromIntegral y)
    plus (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) + y
    plus (IntLit x) (IntLit y) = IntLit $ x + y
mkBinOp '-' = (sub, "-")
  where
    sub (FloatLit x) (FloatLit y) = FloatLit $ x - y
    sub (FloatLit x) (IntLit y) = FloatLit $ x - (fromIntegral y)
    sub (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) - y
    sub (IntLit x) (IntLit y) = IntLit $ x - y
mkBinOp '*' = (times, "*")
  where
    times (FloatLit x) (FloatLit y) = FloatLit $ x * y
    times (FloatLit x) (IntLit y) = FloatLit $ x * (fromIntegral y)
    times (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) * y
    times (IntLit x) (IntLit y) = IntLit $ x * y
mkBinOp '/' = (divide, "/")
  where
    divide (FloatLit x) (FloatLit y) = FloatLit $ x / y
    divide (FloatLit x) (IntLit y) = FloatLit $ x / (fromIntegral y)
    divide (IntLit x) (FloatLit y) = FloatLit $ (fromIntegral x) / y
    divide (IntLit x) (IntLit y) = IntLit $ x `div` y

eval :: MathExpr -> MathExpr
eval (Unary (op, _) x) = op (eval x)
eval (Binary (op, _) x y) = op (eval x) (eval y)
eval (Print e1 e2) = unsafePerformIO ((putStrLn (show (eval e1))) >> return (eval e2))
eval x = x

expr :: Parser MathExpr
expr = chainl1 (space *> term <* space) (Binary . mkBinOp <$> oneOf "+-")
     <|> literal "print" *> do { e1 <- expr
                               ; e2 <- expr
                               ; return $ Print e1 e2}

term :: Parser MathExpr
term = chainl1 (space *> factor <* space) (Binary . mkBinOp <$> oneOf "*/")

factor :: Parser MathExpr
factor = asum [ FloatLit <$> float
              , IntLit <$> integer
              , Unary . mkOp <$> oneOf "+-" <*> factor
              , between (char '(') expr (char ')')
              ]
