{
module Tokens where
}

%wrapper "basic"

tokens :-
  [\n]+                   { \s -> TokenNewline s }
  $white+                 ;
  \(                      { \s -> TokenLParen }
  \)                      { \s -> TokenRParen }
  \=                      { \s -> TokenEqual }
  \|                      { \s -> TokenPipe }
  \,                      { \s -> TokenComma }
  \#                      { \s -> TokenPound }
  \!                      { \s -> TokenExclam }             
  \+                      { \s -> TokenPlus }
  \-                      { \s -> TokenMinus }
  \*                      { \s -> TokenTimes }
  [a-zA-Z][a-zA-Z0-9]*    { \s -> TokenVar s }
  [0-9]+                  { \s -> TokenInt s }
{

-- The token type:
data Token 
  = TokenNewline String
  | TokenLParen 
  | TokenRParen 
  | TokenEqual
  | TokenPipe
  | TokenComma
  | TokenPound
  | TokenExclam
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenVar String
  | TokenInt String
  deriving (Eq,Show)

}
