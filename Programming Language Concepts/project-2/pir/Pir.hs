module Pir where

type Body = [Statement]
type Program = [Line]

data Reference
    = Variable String
    | Projection Reference String
    | IntRef String
    | NullRef
    deriving(Read,Show)

data Expression
    = FunCall String Reference Reference
    | Test Reference
    | Pair Reference Reference
    | Add Reference Reference
    | Sub Reference Reference
    | Mult Reference Reference
    | ExpressionReference Reference
    | IntLit String
    | Null
    deriving(Read,Show)

data Statement
    = If Expression Body 
    | Else Body
    | Assignment Reference Expression
    | Return Expression
    deriving(Read,Show)

data Function
    = Function String Reference Reference Body
    deriving(Read,Show)

data Line
    = Func Function
    | Exclam
    | Assign Statement
    | TestLine Expression Expression
    deriving(Read,Show)
