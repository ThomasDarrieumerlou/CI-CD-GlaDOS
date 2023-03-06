{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Cpt.hs
-}

module Cpt.Cpt (
    Cpt (..),
    Keyword (..),
    getIdentifier, getKeyword, getLiteral, getList, getOperator
  ) where

import Cpt.Literal (Literal)
import Cpt.Operator (Operator)

data Keyword = If | Else | Then | Lambda deriving (Eq)
instance Show Keyword where
  show If = "if"
  show Else = "else"
  show Then = "then"
  show Lambda = "lambda"


data Cpt
  = Literal Literal
  | Identifier String
  | Keyword Keyword
  | Operator Operator
  | Expression [Cpt]
  | Assignement (String, [Literal], [Cpt])
  | Prototype (String, [Literal])
  deriving (Eq, Show)

getIdentifier :: Cpt -> Maybe String
getIdentifier (Identifier s) = Just s
getIdentifier _ = Nothing

getLiteral :: Cpt -> Maybe Literal
getLiteral (Literal x) = Just x
getLiteral _ = Nothing

getKeyword :: Cpt -> Maybe Keyword
getKeyword (Keyword k) = Just k
getKeyword _ = Nothing

getOperator :: Cpt -> Maybe Operator
getOperator (Operator o) = Just o
getOperator _ = Nothing

getList :: Cpt -> Maybe [Cpt]
getList (Cpt.Expression l) = Just l
getList _ = Nothing

-- Faire une fonction qui crée l'abre correspondant à une expression en
-- ajoutant les priorités.

-- Les évaluations sont faites au moment de la création de l'Ast, si
-- l'expression est évaluable, sinon elle est simplement stockée dans
-- la table des symboles.