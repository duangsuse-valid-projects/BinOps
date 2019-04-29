-- | Binary Operation Calculator
-- |
-- | Supported by Text.ParserCombinators.ReadP.ReadP
-- |
-- | Supports:
-- |   Number: Double, negative, hexadecimal
-- |   Operation: + - * / ** mod
-- |   Unary operation: unary- logN sin cos tan
module Main where

-- duangsuse, Mon Apr 29, 2019 年熬夜 3 小时 + 连续 5 小时不休息写成...
-- 新年快乐？（...

import Data.Maybe
import Control.Monad (forever)

--import Text.ParserCombinators.Parsec.Number

import Control.Monad.ST
import Data.STRef
import Control.Exception

import System.Exit
import System.Console.Readline

import BinOps.Model
import BinOps.Eval
import BinOps.Parser

--
bParse :: String -> Maybe Ast
bParse = parse exprP

valid :: String -> Bool
valid s = case bParse s of
  Just _ -> True
  Nothing -> False

calc :: String -> Double
calc = eval . calc' . bParse
    where
        calc' :: Maybe Ast -> Ast
        calc' (Just ast) = ast
        calc' Nothing = (Rat 0.0)

pretty :: String -> String
pretty = show . bParse

-- | Main Calculator REPL
main :: IO ()
main = do
  putStrLn "Sigma :: + - * / ** mod unm logN sin cos tan 0xFF_FF 2.1"
  forever $ catch (do
    maybeLine <- readline "Σ "

    case maybeLine of
      Nothing -> return ()
      Just ":exit" -> doExit
      Just ":q" -> doExit
      Just line -> do
        addHistory line
        runCode line) handler

  where
    runCode str = do
      (putStr "We got: ") >> putStrLn str
      validate str
      putStr " = " >> (putStrLn $ pretty str)
      putStr " = "
      putStr . show $ calc str
    handler :: ArithException -> IO ()
    handler e = (putStr $ "[E] " ++ handle' e) >> putStrLn ""
    handle' :: ArithException -> String
    handle' DivideByZero = "Division by zero"
    handle' RatioZeroDenominator = "RatioZero denominator"
    handle' Overflow = "Math Overflow"
    handle' Underflow = "Math Underflow"
    handle' LossOfPrecision = "Bad loss of precision"
    handle' Denormal = "Denormal"
    validate s = if ((not . valid) s) then putStrLn "Parser: cannot read struct" else putStr ""
    doExit = putStrLn ";) Bye" >> exitSuccess

{-
    putPrompet = runST $ do
      k <- newSTRef 0
      putStr "["
      putStr . show . readSTRef $ 0
      modifySTRef 0 (succ . readSTRef k)
      putStr "] "
-}

