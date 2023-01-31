{-
-- EPITECH PROJECT, 2023
-- glados [WSL: Ubuntu-22.04]
-- File description:
-- Evaluation.hs
-}

module Evaluation (evalAst) where

import Ast (Operator (Plus, Minus, Times, Div), Ast (Define, Function, Value, Call, Operator))
import Literal (Literal (Integer))
import Data.Maybe (fromJust)


data BuiltinOperator a = BuiltinOperator {
  function :: Integral a => a -> a -> a
  , minArgs :: Int
  , neutral :: a
}

getOperator :: Integral a => Operator -> BuiltinOperator a
getOperator Plus = BuiltinOperator {function = (+), minArgs = 0, neutral = 0}
getOperator Minus = BuiltinOperator {function = (-), minArgs = 0, neutral = 0}
getOperator Times = BuiltinOperator {function = (*), minArgs = 1, neutral = 1}
getOperator Div = BuiltinOperator {function = div, minArgs = 1, neutral = 1}

extractValue :: Integral a => Ast -> Maybe a
extractValue (Value (Integer i)) = Just $ fromIntegral i
extractValue _ = Nothing

processOperator :: Integral a => BuiltinOperator a -> [Ast] -> Maybe a
processOperator op as
  | length as < minArgs op = Nothing
  | otherwise = Just (foldr (function op) (neutral op) 
    (fromJust (mapM (extractValue . fromJust . evalAst) as)))


evalAst :: Ast -> Maybe Ast
evalAst (Define _ _) = Nothing
evalAst (Value v) = Just (Value v)
evalAst (Function _ _) = Nothing
evalAst (Call _ _) = Nothing
evalAst (Operator op as) = Just (Value (Integer ((\o ->
  fromJust $ processOperator o as) $ getOperator op)))
