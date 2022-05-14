{
module Tokens where
}

%wrapper "basic"

tokens :-
  $white+                       ;
  "--".*                        ;
  "("                           { \s -> TokenLParen }
  ")"                           { \s -> TokenRParen }
  "{"                           { \s -> TokenLBracket }
  "}"                           { \s -> TokenRBracket }
  ","                           { \s -> TokenComma }
  ";"                           { \s -> TokenSemi }
  "."                           { \s -> TokenPeriod }
  "="                           { \s -> TokenEq }
  "+"                           { \s -> TokenPlus }
  "-"                           { \s -> TokenMinus }
  "*"                           { \s -> TokenTimes }
  "!"                           { \s -> TokenExclam }
  "?"                           { \s -> TokenQuestion }
  "if"                          { \s -> TokenIf }
  "else"                        { \s -> TokenElse }
  "null"                        { \s -> TokenNull }
  "return"                      { \s -> TokenReturn }
  [a-zA-Z][a-zA-Z0-9]*          { \s -> TokenVar s }
  [0-9]+                        { \s -> TokenIntLit s }

{

-- The token type:
data Token = TokenLParen 
           | TokenRParen 
           | TokenLBracket 
           | TokenRBracket 
           | TokenComma 
           | TokenSemi 
           | TokenPeriod 
           | TokenEq 
           | TokenPlus 
           | TokenMinus 
           | TokenTimes 
           | TokenExclam 
           | TokenQuestion 
           | TokenIf 
           | TokenElse 
           | TokenNull 
           | TokenReturn
           | TokenVar String
           | TokenIntLit String
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
