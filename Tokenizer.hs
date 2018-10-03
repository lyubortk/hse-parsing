module Tokenizer where

data Token = TNumber Integer
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Mult
              | Div
              | Pow
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      = let (number, cs') = separate isDigit (c : cs) in
                                       TNumber (read number :: Integer) : tokenize cs'
                  | isAlpha c      = let (ident, cs') = separate isAlphaOrDigit (c : cs) in
                                       TIdent ident : tokenize cs'
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])

separate :: (a -> Bool) -> [a] -> ([a], [a])
separate f s = (takeWhile f s, dropWhile f s)

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
           | c == '^' = Pow
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit x = x `elem` "0123456789"

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z']

isAlphaOrDigit :: Char -> Bool
isAlphaOrDigit c = (isAlpha c) || (isDigit c)

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"
