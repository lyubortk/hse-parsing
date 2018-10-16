module Parser (parse) where -- only expose the top-level parsing function

import Combinators
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem, exp)
import Data.Char (isSpace)

data AST = AExprGroup AST AST
         | AConcat AST AST
         | AList AST
         | AListCore AST AST
         | AAssign String AST
         | ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AExp AST AST
         | ANum Integer
         | AIdent String
         | ANegation AST
         | AEps

-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result (AST, String))
parse [] = Nothing
parse a  = Just ((exprgroup >>= \r -> isEmpty |> return r) a)

exprgroup :: Parser AST
exprgroup = 
  baseexpr >>= \l ->
  ( ( 
        (char ';') |> exprgroup >>= \r -> return (AExprGroup l r)
    )  
    <|> return l
  )

baseexpr :: Parser AST
baseexpr =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    baseexpr >>= \e -> return (AAssign i e)
  )
  <|> ( identifier >>= \l ->
        concat_op |>
        listexpr >>= \r -> return (AConcat l r)
      )
  <|> numexpr 
  <|> listexpr 

listexpr :: Parser AST
listexpr = 
  ( identifier >>= \(AIdent i) ->
    assignment |>
    listexpr >>= \e -> return (AAssign i e)
  )
  <|> ( list >>= \l ->
        concat_op |>
        listexpr >>= \r -> return (AConcat l r)
      )
  <|> list

list :: Parser AST
list = 
  ( lbracket |>
    listcore >>= \e ->
    rbracket |> return (AList e)
  )
  <|> identifier
     
listcore :: Parser AST
listcore =
  ( baseexpr >>= \l ->
    comma |>
    listcore >>= \r -> return (AListCore l r)
  )
  <|> baseexpr
  <|> return AEps

numexpr :: Parser AST
numexpr =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    numexpr >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        numexpr >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  exp >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

exp :: Parser AST
exp =
  factor >>= \l ->
  ( ( powOp |>
      exp   >>= \r  -> return (AExp l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    numexpr >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> number
  <|> (char '-') |> factor >>= \e -> return (ANegation e)

number :: Parser AST
number     = map (ANum . T.number) (sat T.isNumber elem)

identifier :: Parser AST
identifier = map AIdent (sat T.isIdent elem)

lbracket :: Parser Char
lbracket = char '['

rbracket :: Parser Char
rbracket = char ']'

comma :: Parser Char
comma = char ','

concat_op :: Parser String
concat_op = sat (== "++") (parseFirstN 2)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

powOp :: Parser Char
powOp     = char '^'


instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r   -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r   -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e   -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i       -> show i
                  ANegation a    -> showOp T.Minus : "\n" ++ show' (ident n) a 
                  AExp l r       -> showOp T.Pow : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AExprGroup l r -> showOp T.SemiCol : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AConcat l r    -> "++" ++ "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AListCore l r  -> ',' : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AIdent i       -> id i
                  AList i        -> "[..]" ++ "\n" ++ show' (ident n) i
                  AEps           -> "\\eps")
      ident = (+1)
      showOp T.Plus    = '+'
      showOp T.Minus   = '-'
      showOp T.Mult    = '*'
      showOp T.Div     = '/'
      showOp T.Pow     = '^'
      showOp T.SemiCol = ';' 
