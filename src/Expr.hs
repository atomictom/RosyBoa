module Expr where

import Parser
import Control.Applicative
import Control.Monad
import Data.Foldable
import Text.Printf

type BinOp = (MathExpr -> MathExpr -> MathExpr, String)
type Op = (MathExpr -> MathExpr, String)

data MathExpr = Binary BinOp MathExpr MathExpr
          | Unary Op MathExpr
          | FloatLit Float
          | IntLit Integer

instance Show MathExpr where
    show (Binary (_, opName) x y) =
      printf "Binary (%s `%s` %s)" (show x) opName (show y)
    show (Unary (_, opName) x) = printf "Unary (`%s` %s)" opName (show x)
    show (FloatLit f) = show f
    show (IntLit i) = show i

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
eval x = x

expr2 :: Parser MathExpr
expr2 = do
    space
    x <- term
    f <- expr2'
    return (f x)

expr2' :: Parser (MathExpr -> MathExpr)
expr2' = do
    space
    y <- optional ((,) <$> (space *> oneOf "+-" <* space) <*> term)
    space
    case y of
      Nothing -> return id
      Just (c, t) -> do
        f <- expr2'
        return $ \acc -> f (Binary (mkBinOp c) acc t)

expr :: Parser MathExpr
expr = do
    space
    x <- term
    y <- optional ((,) <$> (space *> oneOf "+-" <* space) <*> expr)
    space
    return $ case y of
               Nothing -> x
               Just (c, e) -> Binary (mkBinOp c) x e

term :: Parser MathExpr
term = do
    space
    x <- factor
    y <- optional ((,) <$> (space *> oneOf "*/" <* space) <*> term)
    space
    return $ case y of
               Nothing -> x
               Just (c, t) -> Binary (mkBinOp c) x t

factor :: Parser MathExpr
factor = asum [ FloatLit <$> float
              , IntLit <$> integer
              , Unary . mkOp <$> oneOf "+-" <*> factor
              , between (char '(') (space *> expr <* space) (char ')')
              ]
