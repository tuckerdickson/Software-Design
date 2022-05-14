module Tup where

type Patterns = [Pattern]
type Program = [Line]

data Pattern
  = PatternVar String
  | PatternNull
  | PatternTuple Pattern Pattern
  | PatternTriple Pattern Pattern Pattern
  deriving(Read,Show)

data Expression
  = ExpressionVar String
  | ExpressionInt String
  | ExpressionNull
  | ExpressionTuple Expression Expression
  | ExpressionTriple Expression Expression Expression
  | FunCall2 String Expression Expression
  | FunCall1 String String String
  | Add Expression Expression
  | Sub Expression Expression
  | Mult Expression Expression
  deriving(Read,Show)

data Equation
  = WithoutGuard String Patterns Expression
  | WithGuard String Patterns Expression Expression
  deriving(Read,Show)

data Line
  = Equ Equation
  | Newline
  | Exclam
  | Test Expression Expression
  deriving(Read,Show)
  
