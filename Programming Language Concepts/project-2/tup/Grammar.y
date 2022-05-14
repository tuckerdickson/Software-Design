{
module Grammar where
import Tokens
import Tup
}

%name parseTup
%tokentype { Token }
%error { parseError }

%token
    newline                  { TokenNewline $$ }
    '('                      { TokenLParen }
    ')'                      { TokenRParen }
    '='                      { TokenEqual }
    '|'                      { TokenPipe }
    ','                      { TokenComma }
    '#'                      { TokenPound }
    '!'                      { TokenExclam }             
    '+'                      { TokenPlus }
    '-'                      { TokenMinus }
    '*'                      { TokenTimes }
    var                      { TokenVar $$ }
    int                      { TokenInt $$ }


%%

Program
    : Line Program                                         { [$1] ++ $2 }
    | {- empty -}                                          { [] }
    
Line
    : Equation                                             { Equ $1 }
    | newline                                              { Newline }
    | '!'                                                  { Exclam }
    | Expression '=' Expression                            { Test $1 $3 }

Equation
    : var Patterns '=' Expression                          { WithoutGuard $1 $2 $4 }
    | var Patterns '|' Expression '=' Expression           { WithGuard $1 $2 $4 $6 }

Expression
    : var var var                                          { FunCall1 $1 $2 $3 }
    | var Expression Expression                            { FunCall2 $1 $2 $3 }
    | '(' Expression ',' Expression ')'                    { ExpressionTuple $2 $4 }
    | '(' Expression ',' Expression ',' Expression ')'     { ExpressionTriple $2 $4 $6 }
    | Expression '+' Expression                            { Add $1 $3 }
    | Expression '-' Expression                            { Sub $1 $3 }
    | Expression '*' Expression                            { Mult $1 $3 }
    | var                                                  { ExpressionVar $1 }
    | int                                                  { ExpressionInt $1 }
    | '#'                                                  { ExpressionNull }

Patterns
    : Pattern Patterns              { [$1] ++ $2 }
    | {- empty -}                   { [] }

Pattern
    : '(' Pattern ',' Pattern ',' Pattern ')'   { PatternTriple $2 $4 $6 }
    | '(' Pattern ',' Pattern ')'               { PatternTuple $2 $4 }
    | var                                       { PatternVar $1 }
    | '#'                                       { PatternNull }


{

parseError :: [Token] -> a
parseError tks = error ("Parse error: " ++ show tks)

}
