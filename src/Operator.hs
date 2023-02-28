module Operator (Operator (..), OperatorType (..)) where
import GHC.Generics (Associativity)

type Precedence = Int
data OperatorType
  = Plus
  | Minus
  | Times
  | Div
  | Mod
  deriving (Show, Eq)

data Operator = Operator OperatorType Precedence Associativity deriving (Eq, Show)