module Combinators where
-- Make sure that the names don't clash
import Prelude hiding (lookup, (>>=), map, pred, return, elem)
import Data.Char (isSpace, isAlphaNum)
import Tokenizer ((|||), isUnderscore)

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
             
-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp ->
  case p (dropWhile isSpace inp) of
    Error _ -> q (dropWhile isSpace inp)
    result  -> result

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p (dropWhile isSpace inp) of
    Success (r, inp') -> q r (dropWhile isSpace inp')
    Error err -> Error err

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops off the first AlphaNum sequence of the string
elem :: Parser String
elem str = let (f, s) = span (isAlphaNum ||| isUnderscore) str in
             case f of
               (x:xs) -> Success (f, s)
               [] -> Error ("Substring '" ++ str  ++
                            "' does not start with [0-9a-zA-Z_]")

-- Checks whether the string is empty
isEmpty :: Parser String
isEmpty [] = Success ([],[])
isEmpty a  = Error a

-- Chops off first n characters
parseFirstN :: Int -> Parser String
parseFirstN n cs = case (n <= length cs) of
                     True  -> Success (take n cs, drop n cs)
                     False -> Error ("Can't chop off '"      ++ 
                                     show n                  ++ 
                                     "' elements of string " ++ 
                                     cs)

-- Chops off the first character
firstChar :: Parser Char
firstChar (c : cs) = Success (c, cs)
firstChar [] = Error "Empty string"

-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) firstChar

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err
