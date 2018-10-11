module Tokenizer where

import Data.Char (isAlpha, isAlphaNum, isDigit)

data Operator = Plus
              | Minus
              | Mult
              | Div
              deriving (Show, Eq)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isNumber :: String -> Bool
isNumber str = all isDigit str

number :: String -> Integer
number str = read str::Integer

isIdent :: String -> Bool
isIdent (c:cs) = (isAlpha c) && (all isAlphaNum cs)
isIdent [] = False

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
