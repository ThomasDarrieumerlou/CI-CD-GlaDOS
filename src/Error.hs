{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Error.hs
-}

module Error (
  AstError (..),
  AstErrorReason (..),
  CptError (..),
  CptErrorReason (..),
  GladosError (..),
  GlobalError (..),
  ParseError (..)
  ) where


-- ------------------------------- Ast errors ------------------------------- --

data AstErrorReason
  = InvalidAstReason
  deriving (Eq)

instance Show AstErrorReason where
  show InvalidAstReason = "This is the reason"


data AstError
  = InvalidAst AstErrorReason String
  deriving (Eq)

instance Show AstError where
  show (InvalidAst r s) = "Invalid usage of " ++ s ++ ": " ++ show r


-- ------------------------------- Cpt errors ------------------------------- --

data CptErrorReason
  = InvalidCptNotExpression
  | InvalidCptNotIdentifier
  | InvalidCptNotLiteral
  | InvalidCptNotKeyword
  | InvalidCptNotOperator
  | InvalidCptNotTreatable
  deriving (Eq)

instance Show CptErrorReason where
  show InvalidCptNotExpression = "Not an expression"
  show InvalidCptNotIdentifier = "Not an identifier"
  show InvalidCptNotLiteral = "Not a literal"
  show InvalidCptNotKeyword = "Not a keyword"
  show InvalidCptNotOperator = "Not an operator"
  show InvalidCptNotTreatable = "Should be an expression, assignment, prototype or operation"


data CptError = InvalidCpt CptErrorReason String
  deriving (Eq)

instance Show CptError where
  show (InvalidCpt r s) = "Parse error on input: " ++ s ++ ": " ++ show r


-- ------------------------------ Parse errors ------------------------------ --

data ParseError = InvalidSynthax
  | Unexpected
  | UnexpectedEnd
  deriving (Eq, Show)


-- ------------------------------ Global errors ----------------------------- --

newtype GlobalError = NotImplemented String
  deriving (Eq)

instance Show GlobalError where
  show (NotImplemented s) = "Not implemented: " ++ s


data GladosError
  = Cpt CptError
  | Ast AstError
  | Parser ParseError
  | Global GlobalError
  deriving (Eq, Show)
