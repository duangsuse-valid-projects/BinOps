{-# LANGUAGE DeriveFunctor #-}

module BinOps.Model where

-- Data

-- 也可以用 Finally Tagless 换成 Typeclass 抽象、用 typeclass instance 写实现，不过我不想

-- | Abstract syntax tree
-- | Eq, Show, Read
data Ast' a
  = Add (Ast' a) (Ast' a)
  | Sub (Ast' a) (Ast' a)
  | Mul (Ast' a) (Ast' a)
  | Div (Ast' a) (Ast' a)
  | Pwr (Ast' a) (Ast' a)
  | Mod (Ast' a) (Ast' a)
--
  | Negate (Ast' a)
  | LogN Int (Ast' a)
  | Sin (Ast' a)
  | Cos (Ast' a)
  | Tan (Ast' a)
--
  | Rat Double
  deriving (Eq, Read, Functor)

type Ast = Ast' Double

-- | Evaluate tree
class Eval a where
  eval :: Ast -> a

--
binOpPretty :: (Show l, Show r) => l -> String -> r -> String
binOpPretty l o r = "(" ++ show l ++ " " ++ o ++ " " ++ show r ++ ")"

instance Show (Ast' a) where
  show (Rat n) = (Prelude.show :: Double -> String) n
  show (Add a b) = binOpPretty a "+" b
  show (Sub a b) = binOpPretty a "-" b
  show (Mul a b) = binOpPretty a "*" b
  show (Div a b) = binOpPretty a "/" b
  show (Pwr a b) = binOpPretty a "**" b
  show (Mod a b) = binOpPretty a "mod" b
  show (Negate n) = "-" ++ show n
  show (LogN n x) = "log" ++ show n ++ " " ++ show x
  show (Sin x) = "sin " ++ show x
  show (Cos x) = "cos " ++ show x
  show (Tan x) = "tan " ++ show x

