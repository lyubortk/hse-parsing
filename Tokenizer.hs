module Tokenizer where

import Data.Char (isAlpha, isDigit, isAlphaNum)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              | SemiCol
              deriving (Show, Eq)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^;"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
           | c == ';' = SemiCol
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isNumber :: String -> Bool
isNumber str = all isDigit str

number :: String -> Integer
number str = read str::Integer

isUnderscore :: Char -> Bool
isUnderscore a = '_' == a

isIdent :: String -> Bool
isIdent (c:cs) = ((isAlpha ||| isUnderscore) c) && 
                 (all (isAlphaNum ||| isUnderscore) cs)
isIdent []     = False

infixl 8 |||
(|||)  :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) f g a = (f a) || (g a)

alpha :: Char -> Char
alpha c = c
