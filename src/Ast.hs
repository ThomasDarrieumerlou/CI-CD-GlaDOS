{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast.hs
-}

module Ast (
  Ast (Define, Value, Lambda, Call, Operator),
  cptToAst,
  listToParams,
  listToAst,
  Params,
) where

import Cpt (Cpt (..), Keyword (..), getIdentifier)
import Stack (Stack (..), empty)
import Literal (Literal (Expression))
import Operator (Operator (..))

type Name = String

type Params = [String]


data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Eq, Show)


type OperatorStack = Stack Cpt


listToParams :: [Cpt] -> Maybe Params
listToParams = mapM getIdentifier

listToArgs :: [Cpt] -> Maybe [Ast]
listToArgs = mapM generateAst

-- symbolToOperator :: String -> Maybe Operator
-- symbolToOperator "+" = Just (Operator.Operator Plus 1)
-- symbolToOperator "-" = Just (Operator.Operator Minus 1)
-- symbolToOperator "*" = Just (Operator.Operator Times 2)
-- symbolToOperator "/" = Just (Operator.Operator Div 2)
-- symbolToOperator _ = Nothing

symbolToAst :: String -> Maybe Ast
symbolToAst s = Just (Call s [])

keywordToAst :: Keyword -> Maybe Ast
keywordToAst _ = Nothing

-- defineToAst :: [Cpt] -> Maybe Ast
-- defineToAst [Identifier a, b] = generateAst b >>= (Just . Define a)
-- defineToAst [List (Identifier n:ps), b] = listToParams ps >>=
--     (\params -> generateAst b >>= (Just . Function params)) >>= (Just . Define n)
-- defineToAst _ = Nothing

listToAst :: [Cpt] -> Maybe Ast
listToAst [Keyword Cpt.Lambda, List ps, b] = listToParams ps >>= (\params ->
  generateAst b >>= (Just . Ast.Lambda params))
listToAst (Identifier s:ps) = listToArgs ps >>= (Just . Call s)
-- listToAst _ = Nothing

operatorToAst :: Operator -> Maybe Ast
operatorToAst _ = Nothing


reversePolishCpt :: Cpt -> OperatorStack -> Cpt
reversePolishCpt (List l) s = List l
reversePolishCpt cpt s = cpt


generateAst :: Cpt -> Maybe Ast
generateAst (Literal i) = Just (Value i)
generateAst (Identifier s) = symbolToAst s
generateAst (List l) = listToAst l
generateAst (Keyword k) = keywordToAst k
generateAst (Cpt.Operator o) = operatorToAst o


cptToAst :: Cpt -> Maybe Ast
cptToAst cpt = generateAst $ reversePolishCpt cpt empty
