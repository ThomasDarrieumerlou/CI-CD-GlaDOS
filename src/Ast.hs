{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Ast.hs
-}
{-# LANGUAGE LambdaCase #-}

module Ast (
  Ast (Define, Value, Lambda, Call, Operator),
  cptToAst,
  listToParams,
  listToAst,
  shuntingYard,
  Params,
) where

import Cpt.Cpt (Cpt (..), getIdentifier)
import Cpt.Keyword (Keyword (..))
import Cpt.Literal (Literal (..))
import Cpt.Operator (Operator (..))

type Name = String

type Params = [String]


data Ast
  = Define Name Ast             -- Define a new variable or function
  | Value Literal               -- Value, either Bool, Int, Fraction or Float
  | Lambda Params Ast           -- Takes Args and Body
  | Call Name [Ast]             -- Look for name in bindings and pass args if needed
  | Operator Operator [Ast]     -- Basic operators with its parameters
  deriving (Eq, Show)



listToParams :: [Cpt] -> Maybe Params
listToParams = mapM getIdentifier

listToArgs :: [Cpt] -> Maybe [Ast]
listToArgs = mapM generateAst

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
listToAst [Keyword Cpt.Keyword.Lambda, Cpt.Cpt.Expression ps, b] = listToParams ps >>= (\params ->
  generateAst b >>= (Just . Ast.Lambda params))
listToAst (Identifier s:ps) = listToArgs ps >>= (Just . Call s)
-- listToAst _ = Nothing

operatorToAst :: Operator -> Maybe Ast
operatorToAst _ = Nothing

precedence :: Cpt -> Int
precedence (Cpt.Cpt.Operator (Cpt.Operator.Operator _ p _)) = p
precedence _ = 0

shuntingYard :: Cpt -> Maybe Cpt
shuntingYard (Cpt.Cpt.Expression l) = Cpt.Cpt.Expression <$> shuntingYard' [] [] l where
  shuntingYard' :: [Cpt] -> [Cpt] -> [Cpt] -> Maybe [Cpt]
  shuntingYard' out ops [] = Just $ out ++ ops
  shuntingYard' out ops ((Cpt.Cpt.Literal v):ts) = shuntingYard' (out ++ [Cpt.Cpt.Literal v]) ops ts
  shuntingYard' out ops ((Cpt.Cpt.Expression ls):ts) = shuntingYard (Cpt.Cpt.Expression ls) >>= (\case
    (Cpt.Cpt.Expression xs) -> shuntingYard' (out ++ reverse xs) ops ts
    cpt -> shuntingYard' (out ++ [cpt]) ops ts)
  shuntingYard' out ops (op:ts) = shuntingYard' newOut newOps ts where
    (topOps, restOps) = span (\c -> precedence c >= precedence op) ops
    newOut = out ++ reverse topOps
    newOps = op : restOps
shuntingYard cpt = Just cpt

generateAst :: Cpt -> Maybe Ast
generateAst (Literal i) = Just (Value i)
generateAst (Identifier s) = symbolToAst s
generateAst (Cpt.Cpt.Expression l) = listToAst l
generateAst (Keyword k) = keywordToAst k
generateAst (Cpt.Cpt.Operator o) = operatorToAst o

cptToAst :: Cpt -> Maybe Ast
cptToAst cpt = shuntingYard cpt >>= generateAst
