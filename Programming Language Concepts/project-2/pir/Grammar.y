{
module Grammar where
import Tokens
import Pir
}

%name parsePir
%tokentype { Token }
%error { parseError }

-- names to use in this file for the various Tokens
%token
    '('     { TokenLParen }
    ')'     { TokenRParen }         
    '{'     { TokenLBracket }
    '}'     { TokenRBracket }
    ','     { TokenComma }
    ';'     { TokenSemi }
    '.'     { TokenPeriod }
    '='     { TokenEq }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '!'     { TokenExclam }
    '?'     { TokenQuestion }
    if      { TokenIf }
    else    { TokenElse }
    null    { TokenNull }
    return  { TokenReturn }
    var     { TokenVar $$ }
    int     { TokenIntLit $$ }

%%

Program
    : Line Program      { [$1] ++ $2 }
    | {- empty -}       { [] }

Line
    : Function                              { Func $1 }
    | '!'                                   { Exclam }
    | Statement                             { Assign $1 }
    | Expression '=' Expression             { TestLine $1 $3 }

Function
    : var '(' Reference ',' Reference ')' '{' Body '}'      { Function $1 $3 $5 $8 }

Body
    : Statement Body                            { [$1] ++ $2 }
    | {- empty -}                               { [] }

Statement
    : if Expression '{' Body '}'                { If $2 $4 }
    | else '{' Body '}'                         { Else $3 }
    | Reference '=' Expression ';'              { Assignment $1 $3 }
    | return Expression ';'                     { Return $2 }

Expression
    : var '(' Reference ',' Reference ')'       { FunCall $1 $3 $5 }
    | null '?' '(' Reference ')'                { Test $4 }
    | '(' Reference ',' Reference ')'           { Pair $2 $4 }
    | Reference '+' Reference                   { Add $1 $3 }
    | Reference '-' Reference                   { Sub $1 $3 }
    | Reference '*' Reference                   { Mult $1 $3 }
    | Reference                                 { ExpressionReference $1 }
    | int                                       { IntLit $1 }
    | null                                      { Null }

Reference
    : Reference '.' int       { Projection $1 $3 }
    | var                     { Variable $1 }
    | int                     { IntRef $1 }
    | null                    { NullRef }

{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
