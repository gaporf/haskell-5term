{-# LANGUAGE InstanceSigs #-}

module Expression
  (
    Expr (Const, Add, Sub, Mul, Div, Pow)
  , ArithmeticError (ArithmeticError)
  , eval
  ) where

data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

newtype ArithmeticError = ArithmeticError String
  deriving (Eq, Show)

checkZero
  :: Int
  -> Either ArithmeticError Int
checkZero 0 = Left (ArithmeticError "Division by zero")
checkZero x = Right x

checkNegative
  :: Int
  -> Either ArithmeticError Int
checkNegative x
  | x < 0 = Left (ArithmeticError "Power by negative number")
  | otherwise = Right x

eval
  :: Expr
  -> Either ArithmeticError Int
eval (Const x)        = Right x
eval (Add left right) = eval left >>= \l -> eval right >>= \r -> return $ l + r
eval (Sub left right) = eval left >>= \l -> eval right >>= \r -> return $ l - r
eval (Mul left right) = eval left >>= \l -> eval right >>= \r -> return $ l * r
eval (Div left right) = eval left >>= \l -> eval right >>= checkZero >>= \r -> return $ div l r
eval (Pow left right) = eval left >>= \l -> eval right >>= checkNegative >>= \r -> return $ l ^ r
