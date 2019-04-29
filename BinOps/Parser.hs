module BinOps.Parser where

import Control.Applicative
import Text.ParserCombinators.ReadP
  as P_ hiding (chainl1, chainr1)

import qualified Data.Char as C_

import BinOps.Model

-- Parsers

-- Number: Double, negative, hexadecimal
{- 0-9 . _ - -}
-- Operation
{- + - * / ** mod -}
-- Unary: unary- logN sin cos tan
{- - logNum sin cos tan -}

charP :: Char -> ReadP Char
charP x = satisfy $ (==) x

stringP :: String -> ReadP String
stringP [] = return ""
stringP (x : xs) = do
  _ <- charP x
  cs <- stringP xs
  return $ x : cs

w' :: ReadP Char
w' = satisfy (`elem` " \t\n\r")

ws :: ReadP String
ws = some  w'

ws0 :: ReadP String
ws0 = P_.many  w'


-- Numbers
digitP :: ReadP Char
digitP
  = satisfy C_.isDigit
  +++ charP '.'
  +++ charP '_'

hexDigitP :: ReadP Char
hexDigitP
  = digitP
  +++ satisfy (`elem` "ABCDEFabcdef")

numP :: ReadP Double
numP
  = decNumP
  +++ hexNumP

-- | compute the value from a string of digits using a base
numberValue :: Integral i => Int -> String -> i
numberValue base =
  foldl (\ x -> ((fromIntegral base * x) +) . fromIntegral . C_.digitToInt) 0

hexNumP :: ReadP Double
hexNumP = do
  _ <- stringP "0x"
  digits <- some hexDigitP
  return $ (hexRead . dashTrimH) digits
  where hexRead = fromInteger . numberValue 16
        dashTrimH = filter (not . (== '_'))

decNumP :: ReadP Double
decNumP = do
  isNegate <- (charP '-') <|> return '0'
  decNumNoSignP isNegate

decNumNoSignP :: Read b => Char -> ReadP b
decNumNoSignP prefix = do
  digits <- some digitP
  return $ read $ dashTrimFixH (prefix : digits)
  where
    dashTrimFixH cs = addZero $ filter (not . (== '_')) cs
    addZero ('.' : []) = []
    addZero (c : cs) = c : addZero cs
    addZero [] = []

--
chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 ip op = do
  l <- ip
  rest l
  where
    rest l = do
      o <- op  -- like '+'
      r <- ip  -- like '9'
      rest $ l `o` r -- p {o p}
      <|> return l

chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 p op = scan
  where
    scan   = p >>= rest
    rest l = do
      mid <- op -- like '^'
      r <- scan -- a@(p {op (a|p)})
      return $ l `mid` r
      <|> return l

-- | binary operator abstraction
binOp :: String -> (a -> a -> a) -> ReadP (a -> a -> a)
binOp s = (stringWs0P s >>) . return
  where stringWs0P cs = ws0 >> stringP cs <* ws0

addOp :: ReadP (Ast -> Ast -> Ast)
addOp = binOp "+" Add
subOp :: ReadP (Ast -> Ast -> Ast)
subOp = binOp "-" Sub

mulOp :: ReadP (Ast -> Ast -> Ast)
mulOp = binOp "*" Mul
divOp :: ReadP (Ast -> Ast -> Ast)
divOp = binOp "/" Div

pwrOp :: ReadP (Ast -> Ast -> Ast)
pwrOp = binOp "**" Pwr
modOp :: ReadP (Ast -> Ast -> Ast)
modOp = binOp " mod " Mod

-- Operators
-- + - * / ** mod
exprP :: ReadP Ast
exprP = ws0 >> addSubP

addSubP :: ReadP Ast
addSubP = chainl1 mulDivP (addOp +++ subOp)

mulDivP :: ReadP Ast
mulDivP = chainl1 pwrModP (mulOp +++ divOp)

pwrModP :: ReadP Ast
pwrModP = chainr1 unaryP (pwrOp +++ modOp)

-- Unary operators
-- unary- logNum sin cos tan
unaryP :: ReadP Ast
unaryP = (Rat <$> numP)
  +++ unmP +++ lgNP
  +++ sinP +++ cosP +++ tanP
  +++ parenP

unmP :: ReadP Ast
unmP = kwUnaryP "unm " Negate

lgNP :: ReadP Ast
lgNP = do
  _ <- stringP "log"
  n <- decNumNoSignP ' '
  _ <- ws
  exp' <- exprP
  return $ LogN n exp'

kwUnaryP :: String -> (Ast -> b) -> ReadP b
kwUnaryP k st = do
  _ <- stringP k
  exp' <- exprP
  return $ st exp'

sinP :: ReadP Ast
sinP = kwUnaryP "sin " Sin
cosP :: ReadP Ast
cosP = kwUnaryP "cos " Cos
tanP :: ReadP Ast
tanP = kwUnaryP "tan " Tan

-- Paren
parenP :: ReadP Ast
parenP = do
  _ <- charP '('
  exp' <- exprP
  _ <- charP ')'
  return exp'

-- Helper

-- | Run ReadP Parser with readP_to_S, ignore tracing
runParser :: ReadP a -> String -> [(a, String)]
runParser p s = readP_to_S p s

parse :: ReadP a -> String -> Maybe a
parse p c = case (runParser p c) of
  [] -> Nothing
  xs -> Just . fst . last $ xs
