{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast.hs
-}

module Ast.Ast (
  Ast (Define, Value, Lambda, Call, Operator),
  cptToAst,
  listToParams,
  expressionToAst,
  Params,
) where

import Ast.ShuntingYard (shuntingYard)
import Cpt.Cpt (Cpt (..), getIdentifier)
import Cpt.Keyword (Keyword (..))
import Cpt.Literal (Literal (..))
import Cpt.Operator (Operator (..))
import Error (GlobalError (..), CptError (..), GladosError (..), CptErrorReason (InvalidCptNotTreatable))


-- -------------------------------- Typedefs -------------------------------- --

type Name = String

type Params = [String]

data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Eq, Show)


-- ------------------------ Ast generation functions ------------------------ --


operatorToAst :: Operator -> Either [GladosError] Ast
operatorToAst o = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show o]

keywordToAst :: Keyword -> Either [GladosError] Ast
keywordToAst k = Left [Cpt $ InvalidCpt InvalidCptNotTreatable $ show k]

listToParams :: [Cpt] -> Either [GladosError] Params
listToParams = mapM getIdentifier

listToArgs :: [Cpt] -> Either [GladosError] [Ast]
listToArgs = mapM cptToAst

identifierToAst :: String -> Either [GladosError] Ast
identifierToAst s = Right (Call s [])


-- defineToAst :: [Cpt] -> Maybe Ast
-- defineToAst [Identifier a, b] = cptToAst b >>= (Just . Define a)
-- defineToAst [List (Identifier n:ps), b] = listToParams ps >>=
--     (\params -> cptToAst b >>= (Just . Function params)) >>= (Just . Define n)
-- defineToAst _ = Nothing

expressionToAst :: [Cpt] -> Either [GladosError] Ast
expressionToAst [Keyword Cpt.Keyword.Lambda, Cpt.Cpt.Expression ps, b] = listToParams ps >>= (\params ->
  cptToAst b >>= (Right . Ast.Ast.Lambda params))
expressionToAst (Identifier s:ps) = listToArgs ps >>= (Right . Call s)
expressionToAst e = Left [Global $ NotImplemented $ show e]

operationToAst :: [Cpt] -> Either [GladosError] Ast
operationToAst o = Left [Global $ NotImplemented $ show o]

cptToAst :: Cpt -> Either [GladosError] Ast
cptToAst (Cpt.Cpt.Literal i) = Right (Value i)
cptToAst (Cpt.Cpt.Operation o) = shuntingYard o >>= operationToAst
cptToAst (Cpt.Cpt.Identifier s) = identifierToAst s
cptToAst (Cpt.Cpt.Expression l) = expressionToAst l
cptToAst (Cpt.Cpt.Keyword k) = keywordToAst k
cptToAst (Cpt.Cpt.Operator o) = operatorToAst o
cptToAst (Cpt.Cpt.Prototype _) = Left [Global $ NotImplemented "function prototypes"]
cptToAst (Cpt.Cpt.Assignement _) = Left [Global $ NotImplemented "assignements"]
cptToAst (Cpt.Cpt.Condition _) = Left [Global $ NotImplemented "conditions"]
cptToAst (Cpt.Cpt.Lambda _) = Left [Global $ NotImplemented "lambdas"]
