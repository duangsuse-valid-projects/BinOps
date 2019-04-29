{-# LANGUAGE RankNTypes #-}

module BinOps.Eval where

import GHC.Real
import GHC.Float (integerLogBase)

import BinOps.Model

evalBin :: (Eval a) => (a -> a -> a) -> Ast -> Ast -> a
evalBin op l r = eval l `op` eval r

-- | Calculate step
instance Eval (Double) where
  eval (Rat n) = n
  eval (Add a b) = evalBin (+) a b
  eval (Sub a b) = evalBin (-) a b
  eval (Mul a b) = evalBin (*) a b
  eval (Div a b) = evalBin (/) a b
  eval (Pwr a b) = evalBin (**) a b
  eval (Mod a b) = fromIntegral $ (evalInt a) `mod` (evalInt b)
    where
      evalAndFloor :: forall a. (Integral a) => Ast -> a
      evalAndFloor = floor . (eval :: Ast -> Double)
      evalInt :: Ast -> Integer
      evalInt ast = GHC.Real.toInteger $ evalAndFloor ast
  eval (Negate x) = negate . eval $ x
  eval (LogN n x) = fromIntegral $ (toInteger n) `logBaseII` (floor $ (eval :: Ast -> Double) x)
      where
        logBaseII :: Integer -> Integer -> Int
        logBaseII = integerLogBase
  eval (Sin x) = sin . eval $ x
  eval (Cos x) = cos . eval $ x
  eval (Tan x) = tan . eval $ x
