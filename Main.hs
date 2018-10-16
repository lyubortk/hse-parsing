{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "1-2-3"
  runParser "(((9)))"
  runParser "1*2-3/4+5"
  runParser "!"
  runParser "1 + 2"
  runParser " var = 32^3 + (- 83)/32; a = 43; 44"
  runParser " a = [] ; [b = 13, [z], 42 + 6] ++ a ++ [31, 25]; 777"
